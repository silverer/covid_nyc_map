#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun  8 14:39:33 2020

@author: elisabethsilver
"""

import argparse
import os
import sys
import git
import pandas as pd
import time

def process_df(df_og):
    df = df_og.copy()
    temp = df.dropna(subset = ['POP_DENOMINATOR'])
    temp = temp.drop_duplicates(subset = ['MODIFIED_ZCTA'])
    temp = temp.dropna(subset = ['MODIFIED_ZCTA'])
    
    pop_dict = dict(zip(temp['MODIFIED_ZCTA'].to_list(),
                        temp['POP_DENOMINATOR'].to_list()))
    df.loc[pd.isnull(df['MODIFIED_ZCTA']),
           'MODIFIED_ZCTA'] = df['MODZCTA']
    
    #impute population
    df['POP_DENOMINATOR'] = df['MODIFIED_ZCTA'].map(pop_dict)
    #df['cov_case_rate'] = df['COVID_CASE_RATE']
    #impute case rate
    df.loc[pd.isnull(df['COVID_CASE_RATE']),
           'COVID_CASE_RATE'] = (df['Positive']/df['POP_DENOMINATOR'])*100000
    
    df['COVID_TEST_COUNT'] = df['Total']
    #Impute number of tests
    df.loc[pd.isnull(df['COVID_TEST_COUNT']),
           'COVID_TEST_COUNT'] = round(df['COVID_CASE_COUNT'] / 
                                     (df['PERCENT_POSITIVE']/100))
    
    #df['percent_pos'] = df['PERCENT_POSITIVE']
    #impute percent positive
    df.loc[pd.isnull(df['PERCENT_POSITIVE']),
           'PERCENT_POSITIVE'] = (df['Positive']/df['Total'])*100
    
    df.loc[pd.isnull(df['COVID_CASE_COUNT']),
           'COVID_CASE_COUNT'] = df['Positive']
    
    df['commit_date_time'] = pd.to_datetime(df['commit_date_time'])
    df = df[df['MODIFIED_ZCTA'] != 99999]
    df = df.dropna(subset = ['MODIFIED_ZCTA'])
    return df

def drop_duplicate_commits(df):
    max_zips = len(df['MODIFIED_ZCTA'].drop_duplicates())
    
    no_duplicates = pd.DataFrame()
    grpd = df.groupby(['actual_date'])
    for name, group in grpd:
        temp_df = group.copy()
        keep_time = max(temp_df['commit_date_time'])
        temp_df = temp_df[temp_df['commit_date_time']==keep_time]
        if len(temp_df) > max_zips:
            print('still too long: ', len(temp_df))
            temp_df = temp_df.drop_duplicates(subset = ['MODIFIED_ZCTA'])
        no_duplicates = pd.concat([no_duplicates, temp_df],
                                  ignore_index = True)
    return no_duplicates

def format_commit_time(raw_time):
    if len(str(raw_time[3])) <2:
        hrs = f'0{raw_time[3]}'
    else:
        hrs = str(raw_time[3])
        
    if len(str(raw_time[4])) <2:
        mins = f'0{raw_time[4]}'
    else:
        mins = str(raw_time[4])   
    fmt_time = f"{raw_time[0]}-{raw_time[1]}-{raw_time[2]} {hrs}:{mins}"
    return fmt_time

def format_commit_date(raw_time):
    fmt_time = f"{raw_time[0]}-{raw_time[1]}-{raw_time[2]}"
    return fmt_time
        

def generate_commit_list(repo, commit_ids):
    commit_df = pd.DataFrame()
    commit_df['commit_date'] = 'NaN'
    commit_df['commit_date_time'] = 'NaN'
    i = 0
    for commit_id in commit_ids[::-1]:
        this_commit = repo.commit(commit_id)
        this_tree = this_commit.tree
        this_time = time.gmtime(this_commit.authored_date)
        fmt_date = format_commit_date(this_time)
        commit_df.loc[i, 'commit_id'] = commit_id
        commit_df.loc[i, 'commit_message'] = this_commit.message
        files = []
        for blob in this_tree:
            files.append(blob.path)
            
        commit_df.loc[i, 'commit_files'] = ';'.join(files)
        if 'modzcta' in ';'.join(files):
            commit_df.loc[i, 'filetype'] = 'modzcta'
        elif 'zcta' in ';'.join(files):
            commit_df.loc[i, 'filetype'] = 'zcta'
        #don't keep the previously added commit from the same day
        if fmt_date in commit_df['commit_date'].values:
            commit_df.loc[commit_df['commit_date']==fmt_date,
                          'keep'] = False
        commit_df.loc[i, 'commit_date'] = fmt_date
        commit_df.loc[i, 'commit_date_time'] = format_commit_time(this_time)
        
        i += 1
    return commit_df

def main(args, load = False):
    df = pd.DataFrame()
    try:
        repo = git.Repo(args.path)
    except:
        sys.exit("no such repo")

    try:
        text = repo.git.rev_list(args.branch).splitlines()
    except:
        sys.exit("no such branch")

    commit_ids = text[::args.skip]
    base_url = "https://raw.githubusercontent.com/nychealth/coronavirus-data/"
    get_commits = generate_commit_list(repo, commit_ids)
    get_commits.to_csv('.././data/cleaned_commit_data.csv', 
                       index = False)
    get_commits = get_commits[pd.isnull(get_commits['filetype'])==False]
    get_commits = get_commits[pd.isnull(get_commits['keep'])]
    get_commits = get_commits.set_index(['commit_id'])
    print(f"loaded {len(get_commits)} commits")
    if load:
        df = pd.read_csv('.././data/all_commit_data_dirty.csv')
    else:
        for commit_id in get_commits.index: 
            print('*' * 30)
            print('COMMIT ID: ', commit_id)
            if get_commits.loc[commit_id, 'filetype'] == 'modzcta':
                covid_url = f"{base_url}{commit_id}/data-by-modzcta.csv"
            else:
                covid_url = f"{base_url}{commit_id}/tests-by-zcta.csv"
            date_url = f"{base_url}{commit_id}/case-hosp-death.csv"
            temp = pd.read_csv(covid_url)
            time.sleep(1)
            actual_date = pd.read_csv(date_url)
            time.sleep(1)
            if 'DATE_OF_INTEREST' not in actual_date.columns.to_list():
                print('missing DOI')
                print(actual_date.columns)
            actual_date['DATE_OF_INTEREST'] = pd.to_datetime(actual_date.iloc[:,0])
            actual_date = actual_date.sort_values(by = ['DATE_OF_INTEREST'],
                                                  ascending = False).reset_index()
            temp['actual_date'] = actual_date.loc[0, 'DATE_OF_INTEREST']
            temp['commit_date'] = get_commits.loc[commit_id, 'commit_date']
            temp['commit_date_time'] = get_commits.loc[commit_id, 'commit_date_time']
            temp['commit_id'] = commit_id
            df = pd.concat([df, temp], ignore_index = True)
            print(len(df))
            if len(df) > 0 and len(df) < 600:
                print(df.head())
            print('*'*30)
            print('\n')
        df.to_csv('.././data/all_commit_data_dirty.csv', 
                  index = False)
    new_df = process_df(df)
    new_df = drop_duplicate_commits(new_df)
    new_df.to_csv('.././data/all_time_covid_data.csv', index = False)
    return get_commits, df


if __name__ == '__main__':
    os.chdir('.././coronavirus-data') 
    parser = argparse.ArgumentParser()
    parser.add_argument('path', nargs="?", default=".")
    parser.add_argument('--branch', '-b', default="master")
    parser.add_argument('--skip', '-s', type=int, default=1, help='use every n-th commit')
    args = parser.parse_args()

    get_commits, df = main(args, load = True)
    sys.exit(0)
    
    
    
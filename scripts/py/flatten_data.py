#!usr/bin/env python3

import pickle
import pandas
import file_names


def create_summed_data_matrix(boolean_temporal_df):
    """

    :return:
    """
    append_data = []
    for i in boolean_temporal_df:
        series = boolean_temporal_df[i]
        series[i] = series.sum(axis=1)
        df_new = pandas.DataFrame(series[i])
        append_data.append(df_new)
    append_data = pandas.concat(append_data, axis=1, sort=True)
    append_data.to_csv(file_names.OUTPUTFILE_FLATTEND_DATA + 'flattend_data_02_02_23.csv')


def open_pickl(pkl):
    with open(pkl, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)
    return boolean_temporal_dfs


def main():
    picke_file3 = file_names.PICKLE_FILE
    boolean_temporal_dfs = open_pickl(picke_file3)
    df_t = pandas.DataFrame(boolean_temporal_dfs)
    sum_boolean_temporal_dfs = {donor: pandas.DataFrame(values) * True for donor, values in boolean_temporal_dfs.items()}
    create_summed_data_matrix(sum_boolean_temporal_dfs)

main()

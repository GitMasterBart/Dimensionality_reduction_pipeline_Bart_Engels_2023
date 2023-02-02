import os
import pickle
import pandas
import openpyxl


def create_summed_data_matrix(boolean_temporal_df):
    """

    :return:
    """
    append_data = []
    for i in boolean_temporal_df:
        series = boolean_temporal_df[i]
        series[i] = series.sum(axis=1)
        # print(series[i].keys())    # display(series)
        df_new = pandas.DataFrame(series[i])
        # print(df)
        append_data.append(df_new)
    append_data = pandas.concat(append_data, axis=1, sort=True)
    append_data = append_data.drop(["Year"])
    append_data = append_data.drop(["Year"])
    #append_data = append_data.apply(lambda x: x + 1)
    append_data.to_csv('clinical_trajectories_dictionary_22_12_22.csv')


def create_temporal_data_matrix(boolean_temporal_df):
    df_t_s = []
    df = pandas.read_pickle(boolean_temporal_df)
    for donor, values in df.items():
        df_t = pandas.DataFrame(values)
        df_t_s.append(df_t)
    df_t_s = pandas.concat(df_t_s, axis=1, sort=True)
    df_t_s = df_t_s.groupby(df_t_s.columns, axis=1).sum()
    df_t_s = df_t_s.drop(["Year"])
    df_t_s = df_t_s.drop([-9.0, -1.0], axis=1)
    df_t_s = df_t_s.apply(lambda x: x + 1)
    df_t_s.to_csv('clinical_trajectories_dictionary_22_12_22.csv')

def open_pickl(pkl):
    with open(pkl, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)
    return boolean_temporal_dfs

def main():
    picke_file3 = '../data_umcg/pickle_files/ALL_clinical_trajectories_dictionary_rules_of_thumb_2022-12-15.pkl'
    #
    #print(open_pickl(picke_file1))
    #print(open_pickl(picke_file2))
    ##Convert to df
    boolean_temporal_dfs = open_pickl(picke_file3)

    #df = pandas.read_pickle(boolean_temporal_dfs)

    df_t = pandas.DataFrame(boolean_temporal_dfs)
    df_t.to_csv("ou.csv")
    sum_boolean_temporal_dfs = {donor: pandas.DataFrame(values) * True for donor, values in boolean_temporal_dfs.items()}
    #print(sum_boolean_temporal_dfs)
   # create_temporal_data_matrix(sum_boolean_temporal_dfs)
    create_summed_data_matrix(sum_boolean_temporal_dfs)


main()

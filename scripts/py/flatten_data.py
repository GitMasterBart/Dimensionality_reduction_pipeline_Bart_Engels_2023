import os
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
        # print(series[i].keys())    # display(series)
        df_new = pandas.DataFrame(series[i])
        # print(df)
        append_data.append(df_new)
    append_data = pandas.concat(append_data, axis=1, sort=True)
    # append_data = append_data.drop(["sum"])

    # append_data = append_data.drop(["Year"])
    #append_data = append_data.apply(lambda x: x + 1)
    append_data.to_csv(file_names.OUTPUTFILE_FLATTEND_DATA + 'flattend_data_02_02_23.csv')


def open_pickl(pkl):
    with open(pkl, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)
    return boolean_temporal_dfs

def main():
    picke_file3 = file_names.PICKLE_FILE
    #
    #print(open_pickl(picke_file1))
    #print(open_pickl(picke_file2))
    ##Convert to df
    boolean_temporal_dfs = open_pickl(picke_file3)

    #df = pandas.read_pickle(boolean_temporal_dfs)

    df_t = pandas.DataFrame(boolean_temporal_dfs)
    sum_boolean_temporal_dfs = {donor: pandas.DataFrame(values) * True for donor, values in boolean_temporal_dfs.items()}
    #print(sum_boolean_temporal_dfs)
   # create_temporal_data_matrix(sum_boolean_temporal_dfs)
    create_summed_data_matrix(sum_boolean_temporal_dfs)


main()

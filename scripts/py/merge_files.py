#!usr/bin/env python3

import sys
import ast
import pandas as pd
import pickle
import file_names
import converd_pickle_to_temporalData


def clean_values(x):
    """Convert string representation of list into a list

    Args:
    x (str): string representation of a list

    Returns:
    list: list representation of the string
    """
    return ast.literal_eval(x)[0]


def optain_dataframe(boolean_temporal_dfs):
    """Obtain data frame from the loaded pickle file

        Args:
        boolean_temporal_dfs (dict): a dictionary of dataframes

        Returns:
        pandas.DataFrame: a data frame obtained from the dictionary
        """
    dict_ahh = {}
    # year_dict_summation_death(boolean_temporal_dfs.get("NBB 2020-052"))
    count = 0
    for i in boolean_temporal_dfs:
        # Obtain the data for each year
        dict_ahh[i] = converd_pickle_to_temporalData.year_dict_summation_birth_bucketsize1(boolean_temporal_dfs.get(i), i)

        # Remove the empty values from the dictionary
        for key, value in dict(dict_ahh).items():
            if value is None:
                del dict_ahh[key]

    # Create a data frame from the dictionary
    data_frame = pd.DataFrame.from_dict(dict_ahh, orient="index")
    return data_frame


def merge_df_gen_df_clinical(dfgen, dfclinical):
    """Merge the data frame obtained from the pickle file and the general info

       Args:
       dfgen (pandas.DataFrame): a data frame of general information
       dfgen (pandas.DataFrame): a data frame of general information
       dfclinical (pandas.DataFrame): a data frame obtained from the pickle file

       Returns:
       pandas.DataFrame: a merged data frame
       """
    # Convert the string representation of list into a list
    dfgen['Main_diagnosis'] = dfgen['Main_diagnosis'].apply(clean_values)
    # Merge the two data frames
    return dfclinical.merge(dfgen["Main_diagnosis"], left_index=True, right_index=True)


def change_nan_to_0(df):
    """
    Change NAN values in dataframe to 0

    Arguments:
    df - dataframe

    Returns:
    df - dataframe with NAN values changed to 0
    """
    return df.fillna(0)


def main(argv):
    """
        Main function

        Arguments:
        argv - list of command line arguments

        Returns:
        None
        """
    # Load the pickle file
    picke_file = file_names.PICKLE_FILE
    with open(picke_file, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)

    # Load the general info csv file
    gen_info = pd.read_csv(file_names.GENERAL_INFO, index_col=0)

    # Change NaN values to 0 and obtain the data frame
    df = change_nan_to_0(optain_dataframe(boolean_temporal_dfs))

    # Merge the two data frames
    merge_df = merge_df_gen_df_clinical(gen_info, df)

    # Check if the file is a csv file
    if not sys.argv[1].endswith(".csv"):
        print("Error: No csv file")
    else:
        # Create the output path
        output_path = file_names.OUTPUTFILE_TEMPORAL_DATA + str(sys.argv[1])
        # Write the merged data frame to the output file

        merge_df.to_csv(output_path, index=True)
        print("File is writen to: " + file_names.OUTPUTFILE_TEMPORAL_DATA)


if __name__ == "__main__":
    main(sys.argv[1:])

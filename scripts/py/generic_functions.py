#!usr/bin/env python3

import sys
import ast
import pandas as pd
import pickle
import file_names
import converd_pickle_to_temporalData


def to_hierarchical(df, column):
    """
    This function is used to convert a dataframe to a hierarchical dictionary.
    :param df: The dataframe to convert
    :param column: The column to use as the key
    :return: A hierarchical dictionary
    """

    domains = df["Domain"].unique()

    domains_dict = {x: {} for x in domains}

    # Change the 'ç' to 'c' in the column Parameter
    df["Parameter"] = df["Parameter"].str.replace("ç", "c")

    attributes = df[[column, "Grouping", "Domain", "Parameter"]].values.tolist()
    for group in attributes:
        current_domain = group[2]
        current_grouping = group[1]
        attribute = group[0]
        selected_domain = domains_dict[current_domain]
        if current_grouping in selected_domain:
            selected_domain[current_grouping].append(attribute)

        else:
            selected_domain[current_grouping] = [attribute]

    return domains_dict


def select_domains(domain_list):
    """

    Args:
        domain_list:

    Returns:

    """
    df_attributes = pd.read_excel(file_names.ATTRIBUTE_GROUPING, engine="openpyxl")
    hierarchy = to_hierarchical(df_attributes, "Distinct_attribute")
    symp_list = []
    for domain in domain_list:
        domain_hierachy = hierarchy.get(domain)
        for sign_symp in domain_hierachy:
            symp_list += domain_hierachy.get(sign_symp)
    return symp_list


def optain_dataframe(boolean_temporal_dfs):
    """Obtain data frame from the loaded pickle file
        Args:
        boolean_temporal_dfs (dict): a dictionary of dataframes
        Returns:
        pandas.DataFrame: a data frame obtained from the dictionary
        """
    FILE_LOCATION = (
        "/Users/bengels/Desktop/stage_umcg2022/scripts/Dimensionality_reduction_pipeline_Bart_Engels_2023/Input/Clinical History - attributes grouping in categories - metadata.xlsx")
    df_attributes = pd.read_excel(FILE_LOCATION, engine="openpyxl")
    return df_attributes


def open_pickle(file):
    picke_file = file
    with open(picke_file, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)
    return boolean_temporal_dfs


def remove_empty_values(symptom_dict):
    # Remove the empty values from the dictionary
    for key, value in dict(symptom_dict).items():
        if value is None:
            del symptom_dict[key]
    return symptom_dict


def change_nan_to_0(df):
    """
        Change NAN values in dataframe to 0

        Arguments:
        df - dataframe

        Returns:
        df - dataframe with NAN values changed to 0
        """
    return df.fillna(0)


def clean_values(x):
    """Convert string representation of list into a list

    Args:
    x (str): string representation of a list

    Returns:
    list: list representation of the string
    """
    return ast.literal_eval(x)[0]


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
    dfclinical = dfclinical.merge(dfgen["Main_diagnosis"], left_index=True, right_index=True)
    return dfclinical.merge(dfgen["Gender"], left_index=True, right_index=True)


def check_csv(file_name):
    if not file_name.endswith(".csv"):
        Exception("Error: No csv extension")
    else:
        return file_name

#!usr/bin/env python3


import ast
import pickle
import pandas as pd
import file_names


def to_hierarchical(data_frame, column):
    """
    This function is used to convert a dataframe to a hierarchical dictionary.
    :param data_frame: The dataframe to convert
    :param column: The column to use as the key
    :return: A hierarchical dictionary
    """
    domains = data_frame["Domain"].unique()
    domains_dict = {domain: {} for domain in domains}
    # Change the 'ç' to 'c' in the column Parameter
    data_frame["Parameter"] = data_frame["Parameter"].str.replace("ç", "c")
    attributes = data_frame[[column, "Grouping", "Domain", "Parameter"]].values.tolist()
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
    Domain list: that includes all the domains of interest
    Args:
        domain_list: a list with domains form who the
        signs and symptoms needs to be included in the rest of the analysis.
    Returns:
        A filtered dataframe, with
        only the sign and symptoms
        that are part of the given domains
    """
    df_attributes = pd.read_excel(file_names.ATTRIBUTE_GROUPING, engine="openpyxl")
    hierarchy = to_hierarchical(df_attributes, "Distinct_attribute")
    symp_list = []
    for domain in domain_list:
        domain_hierachy = hierarchy.get(domain)
        for sign_symp in domain_hierachy:
            symp_list += domain_hierachy.get(sign_symp)
    return symp_list

def open_pickle(file):
    """
    open pickle file
    Args:
        file: name + path pickle file
    Returns:
        open pickle file
    """
    picke_file = file
    with open(picke_file, 'rb') as file:
        boolean_temporal_dfs = pickle.load(file)
    return boolean_temporal_dfs


def remove_empty_values(symptom_dict):
    """
    removes empty values from the dict
    Args:
        symptom_dict: donor dict
    Returns:
        symptom dict without empty files

    """
    # Remove the empty values from the dictionary
    for key, value in dict(symptom_dict).items():
        if value is None:
            del symptom_dict[key]
    return symptom_dict


def change_nan_to_0(data_frame):
    """
        Change NAN values in dataframe to 0

        Arguments:
        data_frame - dataframe

        Returns:
        data_frame - dataframe with NAN values changed to 0
        """
    return data_frame.fillna(0)


def clean_values(string_list):
    """Convert string representation of list into a list

    Args:
    x (str): string representation of a list

    Returns:
    list: list representation of the string
    """
    return ast.literal_eval(string_list)[0]


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
    """
    checks if csv has the correct extension
    Args:
        file_name: a string, that represents a
        csv-file name
    Returns:
        the file name if the extension is correct,
        else it returns an error message.
    """
    if not file_name.endswith(".csv"):
        raise Exception("Error: No csv extension")
    return file_name

#!usr/bin/env python3

import flatdict
import re


def dict_to_df_norm_death_bucketsize1(dict):
    """
    This function takes in a dictionary, an interval, and a bucket size. It creates new keys based on the
    difference between the age at death of the patient and the symptom onset year. Keys are created in
    such a way that the difference is divided into buckets of the specified size. The final dictionary is
    returned in a flattened version using flatdict library.

    Parameters:
    dict (dictionary) : The original dictionary
    interval (int)    : The interval used for creating new keys
    bucket_size (int) : Size of the bucket used to divide the difference

    Returns:
    final_dict (flatdict) : The final dictionary in a flattened version using flatdict library

    """
    # Create an empty final_dict
    final_dict = {}
    # Create a list of all values from dict
    list_d = [i for i in dict.values()]
    # Create a list of all keys from dict
    key_list = list(dict.keys())
    # Get the age at death of the patient
    age_of_death = list_d[0]["age_at_death"]
    # If the -9 symptom onset year is present, remove it
    if min(key_list) == -9.0:
        key_list.remove(-9)
    # Create a reversed list of keys
    key_list_rev = list(reversed(key_list))
    # Iterate over the reversed list of keys
    for i in range(len(key_list_rev)):
        # Create a new key based on the difference between age at death and symptom onset year
        title = str(int(list_d[i].get("age_at_death") - key_list[i]))
        # Create a new dictionary with certain keys removed and add it to final_dict
        final_dict[title] = {k: v for k, v in list_d[i].items() if
                             k not in ["neuropathological_diagnosis", "Year", "age_at_death", "sex", "sum"]}
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")


def dict_to_df_norm_birth_bucketsize1(dict, name):

    # Create an empty final_dict
    final_dict = {}
    # Create a list of all values from dict
    list_d = [i for i in dict.values()]
    # Create a list of all keys from dict
    key_list = list(dict.keys())
    date_pattern = re.compile(r"\b\d{4}\b")
    date = re.search(date_pattern, name).group()


    # If the -9 symptom onset year is present, remove it
    if min(key_list) == -9.0:
        key_list.remove(-9)
    # Create a reversed list of keys
    key_list_rev = list(reversed(key_list))

    # Iterate over the reversed list of keys
    for i in range(0, int(list_d[0].get("age_at_death") + 1)):

        # Create a new key based on the difference between age at death and symptom onset year
        title = str(int(0 + i))

        # Create a new dictionary with certain keys removed and add it to final_dict
        try:
            if i in key_list:
                final_dict[title] = {k: v for k, v in list_d[key_list.index(i)].items() if
                                     k not in ["neuropathological_diagnosis", "Year", "age_at_death", "sex", "sum"]}

        except: IndexError
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")

def dict_to_df_norm_death_bucketsizeN(dict_v, bucketsize, domain_dict):
    """
       This function takes in a dictionary, an interval, and a bucket size. It creates new keys based on the
       difference between the age at death of the patient and the symptom onset year. Keys are created in
       such a way that the difference is divided into buckets of the specified size. The final dictionary is
       returned in a flattened version using flatdict library.

       Parameters:
       dict (dictionary) : The original dictionary
       interval (int)    : The interval used for creating new keys
       bucket_size (int) : Size of the bucket used to divide the difference

       Returns:
       final_dict (flatdict) : The final dictionary in a flattened version using flatdict library

       """
    # Create an empty final_dict
    final_dict = {}
    # Create a list of all values from dict
    list_d = [i for i in dict_v.values()]
    # Create a list of all keys from dict
    key_list = list(dict_v.keys())
    if min(key_list) == -9.0:
        key_list.remove(-9)
    # Create a reversed list of keys
    key_list_rev = list(reversed(key_list))
    # Iterate over the reversed list of keys
    if not key_list_rev == []:
        r = [i for i in range(int(max(key_list_rev)), int(min(key_list_rev) - 1), -1)]
        for i in range(0, len(r), bucketsize):
            # Create a new key based on the difference between age at death and symptom onset year
            title = "Year" + str(0 - i)
            result = {}
            if len(r[i:i + bucketsize]) >= 2 and len(r) > bucketsize:
                for x in range((len(r) // bucketsize) + 1):
                    try:
                        dict1 = dict_v.get(int(r[i:i + bucketsize][x]))
                    except IndexError:
                        dict1 = None

                    if dict1 is not None:
                        del dict1["neuropathological_diagnosis"]
                        del dict1["sum"]
                        del dict1["age_at_death"]
                        del dict1["sex"]
                        key_list = []
                        if domain_dict != []:
                            for key in dict1.keys():
                                if key not in domain_dict:
                                    key_list.append(key)
                            for key in key_list:
                                del dict1[key]

                        if not bool(result):
                            result = dict1
                        else:
                            result = {key: result.get(key, 0) + dict1.get(key, 0)
                                      for key in set(result) | set(dict1)}

            else:
                dict1 = dict_v.get(int(r[i:i + bucketsize][0]))
                del dict1["neuropathological_diagnosis"]
                del dict1["sum"]
                del dict1["age_at_death"]
                del dict1["sex"]
                result = dict1
                key_list = []
                if domain_dict != []:
                    for key in dict1.keys():
                        if key not in domain_dict:
                            key_list.append(key)
                    for key in key_list:
                        del dict1[key]
            if not result == {}:
                final_dict[title] = result
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")


def dict_to_df_norm_birth_bucketsizeN(dict, name, bucketsize, domain_dict):

    # Create an empty final_dict
    final_dict = {}
    # Create a list of all values from dict
    list_d = [i for i in dict.values()]
    # Create a list of all keys from dict
    key_list = list(dict.keys())
    date_pattern = re.compile(r"\b\d{4}\b")
    date = re.search(date_pattern, name).group()

    # If the -9 symptom onset year is present, remove it
    if min(key_list) == -9.0:
        key_list.remove(-9)
    # Create a reversed list of keys
    key_list_rev = list(key_list)
    age = int(list_d[0].get("age_at_death"))
    if not key_list_rev == []:
        r = [i for i in range(int(1), int(max(key_list_rev) + 1), +1)]
        for i in range(0, int(age + 1), bucketsize):

            # Create a new key based on the difference between age at death and symptom onset year
            title = str(int(0 + i))
            result = {}
            if len(r[i:i + bucketsize]) >= 2 and len(r) > bucketsize:
                for x in range(((age + 1) // 1) + 1):
                    # print(x)
                    try:
                        # print(r[i:i + bucked_size][x])
                        dict1 = dict.get(int(r[i:i + bucketsize][x]))
                    except IndexError:
                        dict1 = None
                    if dict1 is not None:
                        del dict1["neuropathological_diagnosis"]
                        del dict1["sum"]
                        del dict1["age_at_death"]
                        del dict1["sex"]
                        key_list = []
                        for key in dict1.keys():
                            if key not in domain_dict:
                                key_list.append(key)
                        for key in key_list:
                            del dict1[key]

                        if not bool(result):
                            result = dict1
                        else:
                            result = {key: result.get(key, 0) + dict1.get(key, 0)
                                      for key in set(result) | set(dict1)}
            if not result == {}:
                final_dict[title] = result
            # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")

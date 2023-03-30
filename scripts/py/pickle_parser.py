#!usr/bin/env python3

import re
import flatdict


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

        except:
            IndexError
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")


def dict_to_df_norm_death_bucketsize_n(dict_v, bucketsize, domain_dict):
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
        list_comprehension = [i for i in range(int(max(key_list_rev)), int(min(key_list_rev) - 1), -1)]
        for i in range(0, len(list_comprehension), bucketsize):
            # Create a new key based on the difference between age at death and symptom onset year
            title = "Year" + str(0 - i)
            result = {}
            if len(list_comprehension[i:i + bucketsize]) >= 2 and len(list_comprehension) > bucketsize:
                for index_age in range((len(list_comprehension) // bucketsize) + 1):
                    try:
                        dict1 = dict_v.get(int(list_comprehension[i:i + bucketsize][index_age]))
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
                dict1 = dict_v.get(int(list_comprehension[i:i + bucketsize][0]))
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


def dict_to_df_norm_birth_bucketsize_n(dict, name, bucketsize, domain_dict):
    # Create an empty final_dict
    final_dict = {}
    # Create a list of all values from dict
    list_d = [i for i in dict.values()]
    # Create a list of all keys from dict
    key_list = list(dict.keys())

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
                for index_age in range(((age + 1) // 1) + 1):
                    try:
                        dict1 = dict.get(int(r[i:i + bucketsize][index_age]))
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


def dict_to_df_norm_symptom_bucketsize_n(dict):
    # Create an empty final_dict
    final_dict = {}
    # Create a list of all values from dict
    list_d = [i for i in dict.values()]
    # Create a list of all keys from dict
    key_list = list(dict.keys())
    final_dict = {}
    # If the -9 symptom onset year is present, remove it
    if min(key_list) == -9.0:
        key_list.remove(-9)
    # Create a reversed list of keys
    key_list_rev = list(key_list)
    # pylist_psych = ['Changed_behavior_personality', 'Compulsive_behavior', 'Aggressive_behavior', 'Agitation',
    #                 'Anxiety', 'Changed_moods_emotions', 'Depressed_mood', 'Mania', 'Restlessness', 'Disorientation',
    #                 'Lack_of_insight', 'Wandering', 'Confusion', 'Day_night_rhythm_disturbances', 'Delirium',
    #                 'Delusions', 'Hallucinations', 'Paranoia_suspiciousness', 'Psychosis', 'Psychiatric_admissions',
    #                 'Suicidal_ideation']
    pylist_psych = ["Dementia"]
    start = 0
    if not key_list_rev == []:
        for x in range(0, len(key_list) - 1):
            for i in dict[key_list[x]]:
                if i in pylist_psych and dict[key_list[x]][i] >= 1 and start == 0:
                    start = x
        pos = [i for i,x in enumerate(key_list) if x == start]

        try:
            if int(pos[0]) >= 0 and pos != []:
                for i in range(0,pos[0]):
                    title = str(-start + i)
                    final_dict[title] = {k: v for k, v in dict[key_list[i]].items() if
                                         k not in ["neuropathological_diagnosis", "Year", "age_at_death", "sex", "sum"]}
            for i in range(0, max(key_list)):

                title = str(key_list[i + pos[0]] - start)
                final_dict[title] = {k: v for k, v in dict[key_list[i]].items() if
                                        k not in ["neuropathological_diagnosis", "Year", "age_at_death", "sex", "sum"]}

        except IndexError:
            pass
    return flatdict.FlatDict(final_dict, delimiter=".")


import generic_functions
import file_names

#
def main():
    pickle_file = generic_functions.open_pickle(file_names.PICKLE_FILE)
    # print(pickle_file["NBB 1984-026"])
    dict_to_df_norm_symptom_bucketsize_n(pickle_file["NBB 1992-075"] )
#
#     # donor_dict ={}
#     # for i in pickle_file:
#     #     if dict_to_df_norm_symptom_bucketsize_n(pickle_file[i]) != {}:
#     #         donor_dict[i] = dict_to_df_norm_symptom_bucketsize_n(pickle_file[i])
#     #
#     # print(donor_dict)
#
main()
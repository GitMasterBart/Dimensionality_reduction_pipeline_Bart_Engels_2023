
import os
import sys
import pickle
import pandas as pd
from pandas.io.clipboard import paste

import file_names
import flatdict



def year_dict_summation_bucketsize1(dict):
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
                             k not in ["neuropathological_diagnosis", "Year", "age_at_death", "sex"]}
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")


def year_dict_summation_death_bucketsize2(dict_v):
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
        for i in range(0, len(r), 2):
            # Create a new key based on the difference between age at death and symptom onset year
            title = "Year" + str(0 - i)
            result = {}
            if len(r[i:i + 2]) == 2:
                dict1 = dict_v.get(int(r[i:i + 2][0]))
                dict2 = dict_v.get(int(r[i:i + 2][1]))
                if dict2 != None and dict1 != None:
                    del dict1["neuropathological_diagnosis"]
                    del dict1["Year"]
                    del dict1["age_at_death"]
                    del dict1["sex"]
                    del dict2["neuropathological_diagnosis"]
                    del dict2["Year"]
                    del dict2["age_at_death"]
                    del dict2["sex"]
                    result = {key: dict1.get(key, 0) + dict2.get(key, 0)
                              for key in set(dict1) | set(dict2)}
                if dict1 == None and dict2 != None:
                    del dict2["neuropathological_diagnosis"]
                    del dict2["Year"]
                    del dict2["age_at_death"]
                    del dict2["sex"]
                    result = dict2
                if dict1 != None and dict2 == None:
                    del dict1["neuropathological_diagnosis"]
                    del dict1["Year"]
                    del dict1["age_at_death"]
                    del dict1["sex"]
                    result = dict1
            else:
                del dict_v.get(int(r[i:i + 2][0]))["neuropathological_diagnosis"]
                del dict_v.get(int(r[i:i + 2][0]))["Year"]
                del dict_v.get(int(r[i:i + 2][0]))["age_at_death"]
                del dict_v.get(int(r[i:i + 2][0]))["sex"]
                result = dict_v.get(int(r[i:i + 2][0]))
            if not result == {}:
                final_dict[title] = result
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")


def year_dict_summation_death(dict_v, bucketsize):
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
                        del dict1["Year"]
                        del dict1["age_at_death"]
                        del dict1["sex"]

                        if not bool(result):
                            result = dict1
                        else:
                            result = {key: result.get(key, 0) + dict1.get(key, 0)
                                      for key in set(result) | set(dict1)}

            else:
                dict1 = dict_v.get(int(r[i:i + bucketsize][0]))
                del dict1["neuropathological_diagnosis"]
                del dict1["Year"]
                del dict1["age_at_death"]
                del dict1["sex"]
                result = dict1
            if not result == {}:
                final_dict[title] = result
    # Use flatdict to create a flattened version of final_dict
    return flatdict.FlatDict(final_dict, delimiter=".")



def main(argv):

    picke_file = file_names.PICKLE_FILE

    with open(picke_file, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)
    dict_ahh = {}
    # year_dict_summation_death(boolean_temporal_dfs.get("NBB 2020-052"))
    count = 0
    for i in boolean_temporal_dfs:

        dict_ahh[i] = year_dict_summation_death_bucketsize2(boolean_temporal_dfs.get(i))

        for key, value in dict(dict_ahh).items():
            if value is None:
                del dict_ahh[key]

    df = pd.DataFrame.from_dict(dict_ahh, orient="index")
    if not sys.argv[1].endswith(".csv"):
        print("Error: No csv file")
    else:
        output_path = file_names.OUTPUTFILE_TEMPORAL_DATA + str(sys.argv[1])
        df.to_csv(output_path, index=True)



if __name__ == "__main__":
   main(sys.argv[1:])

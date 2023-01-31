import converd_pickle_to_temporalData
import pickle
import pandas as pd
import ast
import file_names
import sys



def clean_values(x):
    return ast.literal_eval(x)[0]



def optain_dataframe(boolean_temporal_dfs):
    dict_ahh = {}
    # year_dict_summation_death(boolean_temporal_dfs.get("NBB 2020-052"))
    count = 0
    for i in boolean_temporal_dfs:

        dict_ahh[i] = converd_pickle_to_temporalData.year_dict_summation_bucketsize1(boolean_temporal_dfs.get(i))

        for key, value in dict(dict_ahh).items():
            if value is None:
                del dict_ahh[key]

    df = pd.DataFrame.from_dict(dict_ahh, orient="index")
    return df


def merge_dfGen_dfClinical(dfGen, dfClinical):
    dfGen['Main_diagnosis'] = dfGen['Main_diagnosis'].apply(clean_values)
    return dfClinical.merge(dfGen["Main_diagnosis"], left_index=True, right_index=True)



def change_NAN_to_0(df):
    return df.fillna(0)




def main(argv):
    picke_file = file_names.PICKLE_FILE

    with open(picke_file, 'rb') as f:
        boolean_temporal_dfs = pickle.load(f)

    gen_info = pd.read_csv(file_names.GENERAL_INFO, index_col=0)

    df = change_NAN_to_0(optain_dataframe(boolean_temporal_dfs))

    merge_df = merge_dfGen_dfClinical(gen_info, df)
    if not sys.argv[1].endswith(".csv"):
        print("Error: No csv file")
    else:
        output_path = file_names.OUTPUTFILE_TEMPORAL_DATA + str(sys.argv[1])
        merge_df.to_csv(output_path, index=True)
        print("File is writen to: " + file_names.OUTPUTFILE_TEMPORAL_DATA + sys.argv[1])


if __name__ == "__main__":
   main(sys.argv[1:])


#!usr/bin/env python3

"""
Class that transforms a pickle file to a csv file. It imports a file with
generic functions and a parser to transform this to a csv file.
the outcome of this file looks like:
+----------------------------------------------------------------------------------------+
|            {Donors}        |  {years = n }{sign/symptom} | {years = n }{sign/symptom}  |
|         {NBB_YEAR_Case}    |         {occurrence}                {occurrence}          |
+----------------------------------------------------------------------------------------+
there are two normalization options:
Death, Birth
"""

import argparse
import file_names
import pandas as pd
import generic_functions
import pickle_parser
from tqdm import tqdm
import numpy as np


class PickleToTempBirth:
    """
    Input: Pickle file in the form of a dataframe
    Output: temporal csv file with sign and symptoms for n amount of years,
    defined as bucked_size
    Steps: select: sings/symptoms -> put in dataframe ->
    transform to temporal dataframe where year of
    occurrences is linked to sing/symptom
    """

    def __init__(self, domain_list, bucked_size=None, csv_filename=None):
        self.pickle_file = generic_functions.open_pickle(file_names.PICKLE_FILE)
        self.gen_info = pd.read_csv(file_names.GENERAL_INFO, index_col=0)
        self.output_location = file_names.OUTPUTFILE_TEMPORAL_DATA
        self.domain_list = domain_list
        self.bucked_size = bucked_size
        self.data_frame = None
        self.csv_filename = generic_functions.check_csv(csv_filename)

    def set_df(self):
        """
        Transforms the pickle-file into a data-frame that is split in the given bucket size.
        +----------------------------------------------------------------------------------------+
        |            {Donors}        |  {years = n }{sign/symptom} | {years = n }{sign/symptom}  |
        |         {NBB_YEAR_Case}    |         {occurrence}                {occurrence}          |
        +----------------------------------------------------------------------------------------+

        Normalization: Birth
        """
        sign_sym = generic_functions.select_domains(self.domain_list)
        donor_dict = {}
        for donor in tqdm(self.pickle_file):
            if self.bucked_size > 1:
                # Obtain the data for each year
                donor_dict[donor] = pickle_parser. \
                    dict_to_df_norm_birth_bucketsize_n(self.pickle_file.get(donor), donor,
                                                       self.bucked_size, sign_sym)
            else:
                donor_dict[donor] = pickle_parser. \
                    dict_to_df_norm_birth_bucketsize1(self.pickle_file.get(donor), donor)
            generic_functions.remove_empty_values(donor_dict)
        data_frame = pd.DataFrame.from_dict(donor_dict, orient="index")
        data_frame = generic_functions.change_nan_to_0(data_frame)
        self.data_frame = data_frame

    def set_df_gen(self):
        """
        Setter of the general information dataframe.
        This method, add general information to the dataframe
        """
        self.data_frame = generic_functions.merge_df_gen_df_clinical(self.gen_info, self.data_frame)

    def set_csv(self):
        """
        Writes the dataframe that is created to a csv-file.
        While doing this, it displays a loadingbar that tracks the progress.
        """
        csv_file = file_names.OUTPUTFILE_TEMPORAL_DATA + str(self.csv_filename)
        chunks = np.array_split(self.data_frame.index, 100)

        for chunck, subset in enumerate(tqdm(chunks)):
            if chunck == 0:
                self.data_frame.loc[subset].to_csv(csv_file, mode='w', index=True)
            else:
                self.data_frame.loc[subset].to_csv(csv_file, header=None, mode='a', index=True)


class PickleToTempDeath(PickleToTempBirth):
    """
    Input: Pickle file in the form of a dataframe
    Output: temporal csv file with sign and symptoms for n amount of years,
    defined as bucked_size
    Steps: select: sings/symptoms -> put in dataframe ->
    transform to temporal dataframe where year of
    occurrences is linked to sing/symptom
    """

    def set_df(self):
        """
        Transforms the pickle-file into a data-frame that is split in the given bucket size (n).
        +----------------------------------------------------------------------------------------+
        |            {Donors}        |  {years = n }{sign/symptom} | {years = n }{sign/symptom}  |
        |         {NBB_YEAR_Case}    |         {occurrence}                {occurrence}          |
        +----------------------------------------------------------------------------------------+

        Normalization: Death
        """
        sign_sym = generic_functions.select_domains(self.domain_list)
        donor_dict = {}
        for i in tqdm(self.pickle_file):
            if self.bucked_size > 1:
                # Obtain the data for each year
                donor_dict[i] = pickle_parser.dict_to_df_norm_death_bucketsize_n(
                    self.pickle_file.get(i),
                    self.bucked_size, sign_sym)
            else:
                donor_dict[i] = pickle_parser.dict_to_df_norm_death_bucketsize1(
                    self.pickle_file.get(i))
            generic_functions.remove_empty_values(donor_dict)
        data_frame = pd.DataFrame.from_dict(donor_dict, orient="index")
        data_frame = generic_functions.change_nan_to_0(data_frame)
        self.data_frame = data_frame

class PickleToTempPsySymptom(PickleToTempBirth):
    """
    Input: Pickle file in the form of a dataframe
    Output: temporal csv file with sign and symptoms for n amount of years,
    defined as bucked_size
    Steps: select: sings/symptoms -> put in dataframe ->
    transform to temporal dataframe where year of
    occurrences is linked to sing/symptom
    """

    def set_df(self):
        """
        Transforms the pickle-file into a data-frame that is split in the given bucket size (n).
        +----------------------------------------------------------------------------------------+
        |            {Donors}        |  {years = n }{sign/symptom} | {years = n }{sign/symptom}  |
        |         {NBB_YEAR_Case}    |         {occurrence}                {occurrence}          |
        +----------------------------------------------------------------------------------------+

        Normalization: Death
        """
        sign_sym = generic_functions.select_domains(self.domain_list)
        donor_dict = {}
        for i in tqdm(self.pickle_file):
            if pickle_parser.dict_to_df_norm_symptom_bucketsize_n(self.pickle_file.get(i)
                                                                  ) != {}:
                donor_dict[i] = pickle_parser.dict_to_df_norm_symptom_bucketsize_n(
                    self.pickle_file.get(i))
            generic_functions.remove_empty_values(donor_dict)
        data_frame = pd.DataFrame.from_dict(donor_dict, orient="index")
        data_frame = generic_functions.change_nan_to_0(data_frame)
        self.data_frame = data_frame

def main():
    """
    main function; runs the whole code makes it possible to run it from terminal
    """
    # Parse the arguments
    parser = argparse.ArgumentParser(description='')

    parser.add_argument("-d", dest="domain_list" , type=str, nargs="*",
                        default=["Psychiatric", "Cognitive", "General", "Motor", "Sensory"],
                        help="List with domains that are included")
    parser.add_argument("-b", dest="bucket_size", type=int,
                        help="integer that represents the size of the bucket")
    parser.add_argument("-f", dest= "csv_filename", type=str,
                        help="file name of the csv file that is included")
    parser.add_argument("-n", dest= "normalization",  type=str,
                        help="way of normalization")
    args = parser.parse_args()

    # Create the temporal dataframe
    if args.normalization.lower() == "birth":
        temporal_dataframe_transformation = PickleToTempBirth(args.domain_list, args.bucket_size,
                                                              args.csv_filename)
    elif args.normalization.lower() == "death":
        temporal_dataframe_transformation = PickleToTempDeath(args.domain_list, args.bucket_size,
                                                              args.csv_filename)
    elif args.normalization.lower() == "psy":
        temporal_dataframe_transformation = PickleToTempPsySymptom(args.domain_list, args.bucket_size,
                                                              args.csv_filename)
    else:
        # If the normalization is not correct, raise an exception and stop the program
        raise Exception("Give as a fourth argument birth,psy or death.. nothing else")
    temporal_dataframe_transformation.set_df()
    temporal_dataframe_transformation.set_df_gen()
    temporal_dataframe_transformation.set_csv()


if __name__ == "__main__":
    main()

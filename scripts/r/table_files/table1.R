library(Dict)

diseases.trans.dict <- Dict$new('CON' = 'CON', 'TRANS, CON'='CON',
                                'CON, ED'='CON',
                                'SSA, CON'='CON',
                                'AD'='AD',
                                'PD'= 'PD',
                                'PD, DEM'='PDD',
                                'LBV, DEM'='DLB',
                                #                'DEM,SICC,LB,LBV': 'DLB', change 01-08
                                #                'DEM,SICC,LB':'DLB', change 01-08
                                'VD' = 'VD',
                                'FTD, FTD-TDP'='FTD',
                                'FTD, FTD-TDP-A, PROG'='FTD',
                                "FTD, FTD-TAU, TAU"='FTD',
                                'FTD, FTD-FUS'='FTD',
                                'FTD, FTD-TDP-B, C9ORF72'='FTD',
                                'FTD, FTD-UPS'='FTD',
                                'FTD, FTD-TDP-C'='FTD',
                                'FTD, PID'='FTD',
                                'PID, PIDC1, FTD'='FTD',
                                'FTD'='FTD',
                                'PID, PIDC2, FTD'='FTD',
                                
                                'ALS, MND'='MND',
                                'MND'='MND',
                                
                                'PSP' = 'PSP',
                                
                                'SCA, ATAXIA'='ATAXIA',
                                'FRAGX, ATAXIA'='ATAXIA',
                                'ADCA, ATAXIA'='ATAXIA',
                                'FA, ATAXIA'='ATAXIA',
                                
                                'MS, MS_PP'='MS',
                                'MS, MS_SP'='MS',
                                'MS, MS-UN'='MS',
                                'MS, MS_RR'='MS',
                                'MS'='MS',
                                
                                'MSA' = 'MSA',
                                'DEPRI, PSYCH'='MD',
                                'PSYCH, DEPMA'='BP',
                                "SCHIZ, PSYCH"="SCHIZ")
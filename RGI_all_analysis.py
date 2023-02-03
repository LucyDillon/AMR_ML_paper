# Import packages:
import pandas as pd
# Read in MIC & EUCAST data files:
Species_ids = pd.read_csv("All_patric_genus_species.csv")
EUCAST_MIC = pd.read_csv("EUCAST_MIC_values.csv")
MIC_data = pd.read_csv("ALL_patric_MIC_genomes.csv")
drug_class = pd.read_csv("merge_drugclass.csv")
# Read in all genomes:
data = pd.read_csv("ALL_RGI_genomes.csv")
# Convert into a table:
table = pd.pivot_table(data, values=['GeneHit'], index=['genome_id'], columns=['Gene'], fill_value=0)
# MIC analysis:
MIC_reduced = MIC_data[(MIC_data["phenotype"] == "Resistant") | (MIC_data["phenotype"] == "Susceptible")]
# Merge Species_ids and EUCAST_MIC with common column "EUCAST_species"
EUCAST_species_MIC = pd.merge(EUCAST_MIC, Species_ids, on="EUCAST_species")
# Merge EUCAST_species_MIC with original MIC values
Overall_EUCAST_data = pd.merge(left=EUCAST_species_MIC, right=MIC_reduced, how='left', on=['genome_id', 'antibiotic'])
Overall_EUCAST_data = Overall_EUCAST_data.dropna(subset=['phenotype'])
# Take average of MIC values by grouping by genome id and antibiotic
average_Overall_EUCAST_data = Overall_EUCAST_data.groupby(['genome_id', 'antibiotic']).agg({'MIC': ['mean']})
average_Overall_EUCAST_data.columns = ['MIC_mean']
average_Overall_EUCAST_data = average_Overall_EUCAST_data.reset_index()

# merge the average MIC with the overall data to get the extra columns
test_data = pd.merge(average_Overall_EUCAST_data, Overall_EUCAST_data,
                     how='left', left_on=['genome_id', 'antibiotic'],
                     right_on=['genome_id', 'antibiotic'])

# Make a reduced overall dataframe called "Reduced_EUCAST_df"
Reduced_EUCAST_df = test_data[["genome_id", "antibiotic", "MIC_mean", "EUCAST_MIC", "EUCAST_species"]]

# Separate species names and phenotype by separating by underscore delimeter into 2 separate columns
Reduced_EUCAST_df[['EUCAST_species', 'phenotype']] = Reduced_EUCAST_df['EUCAST_species'].str.split('_', expand=True)

# Make two subset tables resistant_EUCAST and susceptible_EUCAST to manipulate
resistant_EUCAST = Reduced_EUCAST_df[Reduced_EUCAST_df["phenotype"] == "Res"]
susceptible_EUCAST = Reduced_EUCAST_df[Reduced_EUCAST_df["phenotype"] == "sus"]

Overall_data = pd.merge(resistant_EUCAST, susceptible_EUCAST, how='left', on=['genome_id',
                                                                              'antibiotic',
                                                                              'MIC_mean',
                                                                              'EUCAST_species'])


def add_phenotype(MIC_mean, Res, Sus):
    if MIC_mean > Res:
        return"Resistant"
    elif MIC_mean <= Sus:
        return"Susceptible"
    else:
        return"Intermediate"


Overall_data['add_phenotype'] = Overall_data.apply(lambda row: add_phenotype(row['MIC_mean'],
                                                                             row['EUCAST_MIC_x'],
                                                                             row['EUCAST_MIC_y']), axis=1)
# merge overall data and drug class table (this is so it can be merged with RGI on genome id and antibiotic)
all_EUCAST_data = pd.merge(Overall_data, drug_class, on="antibiotic")
# Reduce all_EUCAST_data:
all_EUCAST_data_final = all_EUCAST_data[['genome_id', 'antibiotic', 'add_phenotype']]
phenotype = all_EUCAST_data_final[["antibiotic", "genome_id", "add_phenotype"]]
phenotype = phenotype.set_index('genome_id')
# merge phenotype data with eggnog data
final_table = table.join(phenotype)
# only keep rows with susceptible or res phenotype:
final_table = final_table[(final_table["add_phenotype"] == 'Susceptible') |
                          (final_table["add_phenotype"] == 'Resistant')]

Genes = data['Gene']
Genes = Genes.sort_values(ascending=True)
Genes = Genes.drop_duplicates()


def write_to_arff(antibiotic, outfile_antibiotic, final_table):
    antibiotic_df = final_table[final_table["antibiotic"] == antibiotic]
    antibiotic_df = antibiotic_df.reset_index()
    antibiotic_df = antibiotic_df.drop_duplicates()
    antibiotic_df = antibiotic_df.set_index('genome_id')
    antibiotic_df = antibiotic_df.drop('antibiotic', 1)
    antibiotic_array = antibiotic_df.to_string(index=False, header=False)
    antibiotic_array_sep = antibiotic_array.replace(' ', ',')
    antibiotic_array_sep = antibiotic_array_sep.replace(',,,', ',')
    antibiotic_array_sep = antibiotic_array_sep.replace(',,', ',')
    att_out_antibiotic = open(outfile_antibiotic, "w")
    i = 'Gene'
    att_out_antibiotic.write('@RELATION    ' + i + '\n')
    name = 'phenotype'
    for x in Genes:
        att_out_antibiotic.write('@ATTRIBUTE Gene-' + x + '  REAL\n')
    att_out_antibiotic.write('@ATTRIBUTE ' + name + '		{Susceptible, Resistant}\n')
    att_out_antibiotic.write('@DATA\n')
    att_out_antibiotic.write(antibiotic_array_sep + '\n')
    att_out_antibiotic.close()


antibiotics = ['amikacin', 'amoxicillin', 'ampicillin', 'aztreonam', 'cefepime', 'ceftriaxone', 'chloramphenicol',
               'ciprofloxacin', 'clindamycin', 'colistin', 'doripenem', 'ertapenem', 'erthromycin', 'fosfomycin',
               'gentamicin', 'imipenem', 'levofloxacin', 'meropenem', 'moxifloxacin', 'nitrofurantoin', 'tetracycline',
               'tigecycline', 'tobramycin']
               
for antibiotic in antibiotics:
    outfile_antibiotic = f"{antibiotic}_RGI_all_genes.arff"
    write_to_arff(antibiotic, outfile_antibiotic, final_table)

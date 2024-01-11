import pandas as pd
import pickle as pkl

rates=pd.read_excel('modules/Simulations/ed_mh_simulation/function_requirements/Rates5.xlsx').drop_duplicates(subset='Bed_Group',keep='first')
t_beds=rates.groupby('Facility_name')['total_beds'].sum()
n_units=rates.groupby('Facility_name')['Bed_Group'].count()
grouped_df = pd.merge(n_units,t_beds,on='Facility_name').reset_index().to_dict(orient='records')
grouped_df = {'ed_to_ip':grouped_df}

with open(file = 'modules/Simulations/ed_mh_simulation/function_requirements/rates5.pkl', mode = 'wb') as f:
    pkl.dump(grouped_df,f)
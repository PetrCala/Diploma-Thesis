# A script for fetching minimum wage and household median expenditure
# from the world bank online

# How to run
# 1. Into the working directory, add a file called "wage_data_source.csv", where four columns should indicate the year, country name, med household expenditure placeholder, and min wage placeholder. See the existing file for inspiration.
# 2. Run the script.
# 3. In the WD, a file called wage_data_output.csv will be created (if all goes well), and inside will be the updated mhe/min_wage data.

# How it works
# The script calls the worldbank api to obtain the MHE data based on the year and the country name. This is smooth so far. Possibly, it should also be able to call the ILO database to obtain the min wage, but I am not sure whether I will make it work.
# In case the script fails to obtain the data via the API, None (or 0) will be returned instead of the actual value.

import time

import pandas as pd
import requests
import pycountry


API_KEY = 'e7e85108d710932be3ec29a39fb6dd1b' 
COUNTRY_ISOS = {
    "Iran": "Iran, Islamic Republic of",
    "Tanzania": "Tanzania, United Republic of",
    "Palestine": "Palestine, State of",
    "Russia": "Russian Federation",
    "Czech Republic": "Czechia",
}

def main():
    if API_KEY is None:
        raise ValueError("Please provide the ILO database API key first.")
    input_file = "wage_data_source.csv"
    output_file = "wage_data_output.csv"

    data_frame = pd.read_csv(input_file)
    updated_data_frame = update_data_frame(data_frame)
    updated_data_frame.to_csv(output_file, index=False)

    print("Output csv created in the working directory")
    return None

def country_name_to_iso(country_name):
    # Replace with pycountry recognized name if not already
    if country_name in COUNTRY_ISOS.keys():
        country_name = COUNTRY_ISOS.get(country_name)
    # Get ISO
    try:
        country = pycountry.countries.get(name=country_name)
        return country.alpha_2
    except AttributeError:
        print(f"Error: Could not find ISO code for {country_name}")
        return None

def fetch_min_wage_and_expenditures(country_name, year):
    country_iso = country_name_to_iso(country_name)
    if not country_iso:
        return None

    indicators = {
        "minimum_wage": "EAR_4MNT_NOC_RT",
        "median_household_expenditure": "NE.CON.PRVT.PC.KD" # Household final consumption expenditure per capita (constant 2010 US$)
    }

    fetched_data = {}
    for key, indicator in indicators.items():
        if key == "minimum_wage":
            fetched_data[key] = 0
            continue
            url = f"https://www.ilo.org/ilostat/rest/project/indicators/{indicator}/countries/{country_iso}/time_periods?date={year}"
        elif key == "median_household_expenditure":
            url = f"http://api.worldbank.org/V2/country/{country_iso}/indicator/{indicator}?date={year}&format=json"
        else:
            raise ValueError("Invalid key.")

        response = requests.get(url)
        time.sleep(0.2)

        if response.status_code == 200:
            data = response.json()

            if data[1] is not None and len(data[1]) > 0:
                fetched_data[key] = data[1][0]["value"]
                print(data[1][0]["value"]) # verbose 
            else:
                fetched_data[key] = None
        else:
            print(f"Error fetching data for {key}")
            fetched_data[key] = None

    res = {
        "country": country_name,
        "year": year,
        "minimum_wage": fetched_data["minimum_wage"],
        "median_household_expenditure": fetched_data["median_household_expenditure"]
    }

    return res

def update_data_frame(data_frame):
    unique_pairs = data_frame[['data_avgyear', 'country']].drop_duplicates().values.tolist()

    fetched_data = {}
    for pair in unique_pairs:
        year, country = pair
        result = fetch_min_wage_and_expenditures(country, year)
        # Extract data if exists, else put None as a placeholder
        if result:
            mhe = result['median_household_expenditure']
            min_wage = result['minimum_wage']
        else:
            mhe = None
            min_wage = None
        # Add the data to the fetched_data dict
        fetched_data[(year, country)] = {
            'med_exp': mhe,
            'min_wage': min_wage
        }

    for index, row in data_frame.iterrows():
        year, country = row['data_avgyear'], row['country']
        if (year, country) in fetched_data:
            data_frame.at[index, 'med_exp'] = fetched_data[(year, country)]['med_exp']
            data_frame.at[index, 'min_wage'] = fetched_data[(year, country)]['min_wage']

    return data_frame

if __name__ == "__main__":
    main()
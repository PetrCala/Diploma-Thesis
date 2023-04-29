import pycountry
import pandas as pd

def main():
    # missing = get_missing_countries_iso()
    # lookup_missing_countries(missing)
    for country in pycountry.countries:
        print(country)

def get_missing_countries_iso():
    input_file = "wage_data_source.csv"
    data_frame = pd.read_csv(input_file)

    all_countries = data_frame.country.drop_duplicates()
    missing_countries = []
    valid_countries = []
    for country in all_countries:
        try:
            iso = pycountry.countries.get(name=country)
            valid_countries.append(iso.alpha_2)
        except AttributeError:
            print(f"Error: Could not find ISO code for {country}")
            missing_countries.append(country)
    
    return missing_countries

def lookup_missing_countries(missing_countries):
    missing_info = {}
    for country in missing_countries:
        info = pycountry.countries.lookup(country)
        print(info)




if __name__ == "__main__":
    main()


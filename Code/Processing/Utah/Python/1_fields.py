# %%
from pathlib import Path
import pandas as pd
import geopandas as gpd
import numpy as np
import openpyxl

# %%
# Initialize dictionary to store each WRLU year
wrlu_dict = {}

# Load and process each WRLU year
for yr in range(2017, 2025):
    # Shapefile path
    file_path = Path(f"Data/Raw/Fields/Utah/Repaired WRLU/{yr}/wrlu_{yr}.shp")

    # Load and process 2024 separately
    if yr == 2024:
        # Load shapefile
        wrlu_2024 = gpd.read_file(file_path)

        # Only include agricultural fields in Utah
        wrlu_2024 = wrlu_2024[
            (wrlu_2024["Landuse"] == "Agricultural") &
            (wrlu_2024["State"] == "Utah")
        ].copy()

        # Select and rename needed variables
        wrlu_2024 = wrlu_2024[[
            'OBJECTID', 'County', 'Basin', 'SubArea', 'Landuse',
            'Acres', 'Descriptio', 'Class_Name', 'CropGroup',
            'LU_Group', 'IRR_Method', 'geometry'
        ]].rename(columns={
            'OBJECTID': 'id',
            'County': 'county',
            'Basin': 'basin',
            'SubArea': 'sub_area',
            'Landuse': 'land_use',
            'Acres': 'acres_2024',
            'Descriptio': 'crop_2024',
            'Class_Name': 'cdl_2024',
            'CropGroup': 'crop_group_2024',
            'LU_Group': 'land_use_group_2024',
            'IRR_Method': 'irr_method_2024'
        })

        # Make geometry valid
        wrlu_2024["geometry"] = wrlu_2024["geometry"].make_valid()

        # Transform to NAD 83 for spatial operations
        wrlu_2024 = wrlu_2024.to_crs(epsg=26912)

        # Create field centroid for joining with previous years
        wrlu_2024["centroid"] = wrlu_2024["geometry"].centroid

    else: 
        # Load shapefile
        wrlu = gpd.read_file(file_path)

        # Only include agricultural fields in Utah
        wrlu = wrlu[
            (wrlu["Landuse"] == "Agricultural") &
            (wrlu["State"] == "Utah")
        ].copy()

        # Select and rename needed variables
        wrlu = wrlu[[
            'Acres', 'Descriptio', 'Class_Name', 'CropGroup',
            'LU_Group', 'IRR_Method', 'geometry'
        ]].rename(columns={
            'Acres': f'acres_{yr}',
            'Descriptio': f'crop_{yr}',
            'Class_Name': f'cdl_{yr}',
            'CropGroup': f'crop_group_{yr}',
            'LU_Group': f'land_use_group_{yr}',
            'IRR_Method': f'irr_method_{yr}'
        })

        # Make geometry valid
        wrlu["geometry"] = wrlu["geometry"].make_valid()

        # Transform to NAD 83 for spatial operations
        wrlu = wrlu.to_crs(epsg=26912)

        # Store in dictionary with year as key
        wrlu_dict[str(yr)] = wrlu

# %%
# Load crop rooting zone depths
rooting_depth = pd.read_csv("Data/Misc/rooting_depth.csv")

# %%
# Define 2024 fields as base polygons
wrlu_base = wrlu_2024.copy()

# %%
# Loop through each year, spatially joining with 2024 fields
for yr in wrlu_dict.keys():
    # Get current year's WRLU gdf
    wrlu_current = wrlu_dict[yr]

    # Join current year's fields with 2024's field centroids
    wrlu_base = gpd.sjoin(
        wrlu_base,
        wrlu_current,
        how = "left",
        predicate = "intersects"
    )

    # Create dynamic column names for current year
    acres_col = f"acres_{yr}"
    crop_col = f"crop_{yr}"
    cdl_col = f"cdl_{yr}"
    crop_group_col = f"crop_group_{yr}"
    land_use_group_col = f"land_use_group_{yr}"
    irr_method_col = f"irr_method_{yr}"

    # Calculate difference in acres between current year and 2024 field
    wrlu_base["acres_diff"] = abs(wrlu_base["acres_2024"] - wrlu_base[acres_col])

    # For each field, keep the intersecting field with smallest acreage difference
    wrlu_base = (wrlu_base
                 .sort_values("acres_diff")
                 .groupby("id", as_index = False)
                 .first())
    
    # Replace current year's entry with NA if acreage difference is too big
    wrlu_base[crop_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                   np.nan,
                                   wrlu_base[crop_col])
    
    wrlu_base[cdl_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                   np.nan,
                                   wrlu_base[cdl_col])
    
    wrlu_base[crop_group_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                   np.nan,
                                   wrlu_base[crop_group_col])
                                   
    wrlu_base[land_use_group_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                   np.nan,
                                   wrlu_base[land_use_group_col])   
    
    wrlu_base[irr_method_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                   np.nan,
                                   wrlu_base[irr_method_col])
    
    if 'index_right' in wrlu_base.columns:
        wrlu_base = wrlu_base.drop(columns=['index_right'])

# %%

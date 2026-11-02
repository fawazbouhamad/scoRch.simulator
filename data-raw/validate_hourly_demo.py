from pathlib import Path
import argparse
from netCDF4 import Dataset
import numpy as np
import json
parser = argparse.ArgumentParser(description='Independently validate the paired hourly preprocessing demonstration.')
parser.add_argument('folder', type=Path)
folder = parser.parse_args().folder
with Dataset(folder/'era5_hourly_us_2012_07.nc') as nc:
    assert nc['t2m'].shape == (744, 101, 289)
    assert np.all(np.diff(nc['valid_time'][:]) == 1)
    results = []
    for day in range(31):
        # Independent temporal reduction followed by 4x4 spatial block means.
        hourly = np.asarray(nc['t2m'][24*day:24*(day+1)], dtype=np.float64)
        daily = hourly.max(axis=0) - 273.15
        complete = daily[::-1][:100, :288]
        results.append(complete.reshape(25, 4, 72, 4).mean(axis=(1, 3)))
    independent = np.stack(results)
with Dataset(folder/'daily_tmax_us_2012_07.nc') as nc:
    assert nc['tmax'].dimensions == ('time', 'lat', 'lon')
    prepared = np.asarray(nc['tmax'][:])
    assert nc['tmax'].dtype == np.dtype('float64')
error = float(np.max(np.abs(independent-prepared)))
assert error < 1e-12, error
record = {'independent_validation': 'NumPy daily maxima and 4x4 spatial block means',
          'hours': 744, 'daily_fields': 31, 'one_degree_cells': 1800,
          'maximum_absolute_difference_C': error,
          'R_prepared_round_trip': 'exact temperature equality',
          'purpose': 'Preprocessing only; the separate 30-year file supplies the full-analysis demonstration'}
(folder/'validation.json').write_text(json.dumps(record, indent=2)+'\n')
print(json.dumps(record, indent=2))

"""Rebuild the July 2012 full-US hourly preprocessing demonstration.
This maintainer-only script never creates synthetic hourly values. It reads
the documented public Google ARCO-ERA5 archive and retains the original
float32 temperatures in a CDS-compatible NetCDF subset.
"""
from pathlib import Path
import argparse
# Optional maintainer dependencies: numpy, netCDF4 and numcodecs.
import concurrent.futures
import datetime as dt
import hashlib
import json
import time
import urllib.request
import numpy as np
from netCDF4 import Dataset
from numcodecs import get_codec
base = 'https://storage.googleapis.com/gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3'
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('destination', type=Path, help='Output and restart cache directory')
destination = parser.parse_args().destination
destination.mkdir(parents=True, exist_ok=True)
def fetch(path):
    for attempt in range(5):
        try:
            with urllib.request.urlopen(base + '/' + path, timeout=90) as response:
                return response.read()
        except Exception:
            if attempt == 4: raise
            time.sleep(1 + attempt)
def metadata(variable):
    return json.loads(fetch(variable + '/.zarray'))
def read_chunk(variable, key, meta):
    raw = fetch(variable + '/' + key)
    decoded = get_codec(meta['compressor']).decode(raw) if meta['compressor'] else raw
    return np.frombuffer(decoded, dtype=meta['dtype']), len(raw)
meta = metadata('2m_temperature')
assert meta['chunks'] == [1, 721, 1440] and meta['dtype'] == '<f4'
lat, _ = read_chunk('latitude', '0', metadata('latitude'))
lon, _ = read_chunk('longitude', '0', metadata('longitude'))
lat_indices = np.flatnonzero((lat >= 25) & (lat <= 50))
lon_indices = np.flatnonzero((lon >= 230) & (lon <= 302))
assert len(lat_indices) == 101 and len(lon_indices) == 289
origin = dt.datetime(1900, 1, 1)
first = dt.datetime(2012, 7, 1)
hours = np.arange(int((first - origin).total_seconds() // 3600),
                  int((first - origin).total_seconds() // 3600) + 744)
tm = metadata('time')
time_chunk, _ = read_chunk('time', str(int(hours[0] // tm['chunks'][0])), tm)
assert time_chunk[int(hours[0] % tm['chunks'][0])] == hours[0]
def one(hour):
    cache = destination / f'{hour}.npy'
    if cache.exists(): return int(hour), np.load(cache), 0
    data, size = read_chunk('2m_temperature', f'{hour}.0.0', meta)
    field = data.reshape(721, 1440)[np.ix_(lat_indices, lon_indices)].copy()
    assert np.isfinite(field).all()
    np.save(cache, field)
    return int(hour), field, size
out = destination / 'era5_hourly_us_2012_07.nc'
if out.exists():
    raise FileExistsError(f'Choose a different output directory: {out}')
partial = out.with_suffix('.partial.nc')
written = transferred = 0
with Dataset(partial, 'w', format='NETCDF4') as nc:
    for name, length in [('valid_time', len(hours)), ('latitude', len(lat_indices)), ('longitude', len(lon_indices))]:
        nc.createDimension(name, length)
    vtime = nc.createVariable('valid_time', 'i8', ('valid_time',))
    vtime.units = 'hours since 1900-01-01 00:00:00'
    vtime.calendar = 'standard'
    vtime[:] = hours
    vlat = nc.createVariable('latitude', 'f8', ('latitude',)); vlat.units = 'degrees_north'; vlat[:] = lat[lat_indices]
    vlon = nc.createVariable('longitude', 'f8', ('longitude',)); vlon.units = 'degrees_east'; vlon[:] = lon[lon_indices]
    temp = nc.createVariable('t2m', 'f4', ('valid_time', 'latitude', 'longitude'),
                             zlib=True, complevel=6, shuffle=True, chunksizes=(24, 101, 289))
    temp.units = 'K'; temp.long_name = '2 metre temperature'
    with concurrent.futures.ThreadPoolExecutor(max_workers=12) as pool:
        for hour, field, size in pool.map(one, hours):
            temp[hour - hours[0]] = field
            written += 1; transferred += size
            if written % 12 == 0: print(f'{written}/{len(hours)} genuine hourly fields retained', flush=True)
    nc.Conventions = 'CF-1.8'
    nc.source = 'ERA5 2-m temperature from Google ARCO-ERA5, ' + base
    nc.history = 'Spatial subset only; original float32 values, hourly time steps and 0.25-degree grid retained.'
    nc.license = 'Contains Copernicus Climate Change Service information 2026 (ERA5).'
partial.replace(out)
record = {'file': out.name, 'sha256': hashlib.sha256(out.read_bytes()).hexdigest(),
          'bytes': out.stat().st_size, 'transferred_bytes': transferred, 'source': base,
          'period': ['2012-07-01T00:00:00Z', '2012-07-31T23:00:00Z'],
          'area': [50, -130, 25, -58], 'hours': len(hours)}
(destination / 'provenance.json').write_text(json.dumps(record, indent=2))
print(json.dumps(record, indent=2))

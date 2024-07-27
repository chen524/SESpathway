'''
 # @ Author: feng
 # @ Create Time: 2022-10-19 16:01:13
 # @ Modified by: feng
 # @ Modified time: 2022-10-19 16:01:14
 # @ Description: Get ROI measures.
 '''

import os
from os.path import join as opj
from glob import glob
import nibabel as nib
import pandas as pd
import numpy as np
import logging
logging.basicConfig(level=logging.DEBUG)

# >>>>>>>>>>>>>>>>>> run <<<<<<<<<<<<<<<<
proj = 'D:\Projects\SES_T1'
der = opj(proj, 'data', 'ses_t1w_supp2_smoothed')
# meaPrefix = 'GM'
atlasName = 'BN_Atlas_246_1.5mm.nii'
atlasInfo = pd.read_csv(opj(proj, 'atlas', 'BNA_labels.csv'), header=0, index_col=0)
resultPath = opj(proj, 'derivatives', 'GMV_BN_Atlas_Supp2.csv')

result = pd.DataFrame()
for subPath in sorted(glob(opj(der, 'sub-*'))):
    subId = os.path.split(subPath)[-1]
    logging.info(subId)
    
    subAtlasData = nib.load(opj(proj, 'atlas', atlasName)).get_fdata()
    subMeaData = nib.load(opj(subPath, 'mri', f'{subId}_T1w.nii')).get_fdata()
    subRow = {'subid': [subId]}
    for idxRegion in atlasInfo.index.values:
        logging.info(idxRegion)
        subRow[atlasInfo.loc[idxRegion, 'Abbr']] = [np.nansum(subMeaData[subAtlasData==idxRegion])]
    result = pd.concat([result, pd.DataFrame(subRow)])
    # break
    
result.to_csv(resultPath, index=False)
logging.info('Done.')
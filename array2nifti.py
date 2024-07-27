'''
 # @ Author: feng
 # @ Create Time: 2023-03-20 18:15:17
 # @ Modified by: feng
 # @ Modified time: 2023-03-20 18:15:22
 # @ Description:
 '''

import pandas as pd
import numpy as np
import nibabel as nib

infoDf = pd.read_csv("./derivatives/volumn_region.csv", header=0, index_col=2)
img = nib.load("tpl-MNI152NLin2009cAsym_res-01_atlas-AAL90v1_dseg.nii.gz")
data = img.get_fdata()

df = pd.read_csv("gain-AAL90-ttest.csv", header=0, index_col=0)
newData = np.zeros(shape = data.shape)
for i in infoDf.index.values:
    tmpIndex = infoDf.loc[i, "Index"]
    newData[data == tmpIndex] = df.loc[i, "estimate"]

nib.save(nib.Nifti1Image(newData, img.affine), "gain-AAL90-ttest.nii.gz")
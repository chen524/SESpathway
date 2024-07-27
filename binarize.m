%% binarize
addpath('D:\Applications\MATLAB\R2022a\toolbox\spm12');

% Load the NIfTI file
nii = spm_vol('D:\Projects\SES_T1\derivatives\seseffect_cortical.nii.gz');
img = spm_read_vols(nii);

% Binarize the data
img_binarized = img > 0;

% Save the binarized data back to a NIfTI file
nii.fname = 'res_binarized.nii';
spm_write_vol(nii, img_binarized);

%%
% Load the NIfTI file
nii = spm_vol('D:\Projects\SES_T1\derivatives\seseffect_cortical\rres_binarized.nii');

% Get the voxel size
voxel_size = nii.mat(1:3, 1:3);

% Display the voxel size
disp('Voxel size:');
disp(abs(diag(voxel_size)));

%% plot
% draw in enigma tbx
filename = 'D:\Projects\SES_T1\results\ENIGMA\merged_volume.csv'; % Replace with your actual file name
data = readtable(filename);

% Extract the CT_F values
fvalue = data.F;

% Map parcellated data to the surface
fvalue_fsa5 = parcel_to_surface(fvalue, 'aparc_fsa5');

% Project the results on the surface brain
f = figure;
plot_cortical(fvalue_fsa5, 'surface_name', 'fsa5', 'color_range', ...
              [5, 55], 'cmap', 'RdBu_r');
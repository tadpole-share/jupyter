# jupyter

## Setup
- miniconda3
- conda env create -f environment.yml

## Creating conda environment
- use [miniconda3](https://docs.conda.io/en/latest/miniconda.html)
- make sure miniconda bin path is in $PATH
- create virtual env:
  `conda create -n tadpole python=3`
  
- `activate tadpole`
- `conda install -n tadpole -y jupyterlab pandas scikit-learn numpy matplotlib ipywidgets widgetsnbextension`
- `jupyter lab`
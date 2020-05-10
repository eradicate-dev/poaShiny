
# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = c('numpy', 'rios')

# Begin app server
# virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
virtualenv_dir = "test_env"
# python_path = Sys.getenv('PYTHON_PATH')
python_path = "python3"

# Create virtual env and install dependencies
reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)
reticulate::use_virtualenv(virtualenv_dir, required = T)

reticulate::py_run_file("CopyOfKaitake_sc0_Farm.py")
  
calculation <- reticulate::py_run_file("proofofabsence/calculation.py")


calculation$calcProofOfAbsence


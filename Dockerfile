FROM continuumio/miniconda3

RUN mkdir -p /opt/tadpole/notebooks /opt/tadpole/data
WORKDIR /opt/tadpole

ADD environment.yml /opt/tadpole/environment.yml
RUN conda env create -f /opt/tadpole/environment.yml
RUN activate tadpole

# RUN /opt/conda/bin/conda install -n tadpole -y jupyter --quiet
# RUN conda env export -n tadpole --no-builds > environment.yml

ADD notebooks /opt/tadpole/notebooks/
ADD evaluation /opt/tadpole/evaluation/
ADD Data /opt/tadpole/data
ADD tadpole/preprocessing /opt/tadpole/preprocessing

RUN python /opt/tadpole/preprocessing/script_dataPrep.py
RUN python /opt/tadpole/preprocessing/script_dataPrep_D3.py

ENV PATH /opt/conda/envs/tadpole/bin:$PATH
RUN /bin/bash -c "source activate tadpole"
EXPOSE 8888
CMD [ "jupyter", "notebook", "--notebook-dir=/opt/tadpole/notebooks", "--ip='0.0.0.0'", "--port=8888", "--no-browser", "--allow-root" ]


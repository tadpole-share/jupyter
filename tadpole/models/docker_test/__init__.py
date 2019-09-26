from tadpole.models.tadpole_model import TadpoleModel


class DockerTestModel(TadpoleModel):
    def train(self, train_set_path):

        



        client = docker.from_env()
        print(client.containers.run("ubuntu", "ls /"))


        raise Exception('bla')

    def predict(self, test_set_path, datetime):
        pass

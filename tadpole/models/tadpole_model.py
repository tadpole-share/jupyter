from abc import ABC, abstractmethod


class TadpoleModel(ABC):
    @abstractmethod
    def train(self, train_set_path):
        pass

    @abstractmethod
    def predict(self, test_set_path, datetime):
        pass

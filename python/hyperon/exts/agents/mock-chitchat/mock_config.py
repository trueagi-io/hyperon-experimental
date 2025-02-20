import yaml

class MockConfig:
    def __init__(self, fname):
        self.config = yaml.safe_load(open(fname))
    def get(self, field):
        # we need it to fail if field is absent
        return self.config[field]


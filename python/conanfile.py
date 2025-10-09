from conan import ConanFile
from conan.tools.cmake import cmake_layout


class HyperonpyRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps", "CMakeToolchain"

    def requirements(self):
        self.requires("optional-lite/3.5.0")
        self.requires("pybind11/2.10.1")
        if self.settings.os == "Windows":
            self.requires("openssl/3.4.1")

    def layout(self):
        cmake_layout(self)

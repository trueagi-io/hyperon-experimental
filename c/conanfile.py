from conan import ConanFile
from conan.tools.cmake import cmake_layout


class HyperoncRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps", "CMakeToolchain"

    def requirements(self):
        self.requires("libcheck/0.15.2")
        if self.settings.os == "Windows":
            self.requires("openssl/3.4.1")

    def layout(self):
        cmake_layout(self)

    def configure(self):
        self.options["libcheck"].with_subunit = False

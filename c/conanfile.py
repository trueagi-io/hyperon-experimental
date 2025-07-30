from conan import ConanFile
from conan.tools.cmake import cmake_layout


class HyperoncRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps", "CMakeToolchain"

    def requirements(self):
        # Force C++17 standard for protobuf compatibility
        if self.settings.compiler == "gcc":
            self.settings.compiler.cppstd = "gnu17"
        elif self.settings.compiler == "clang":
            self.settings.compiler.cppstd = "gnu17"
        elif self.settings.compiler == "apple-clang":
            self.settings.compiler.cppstd = "17"
        elif self.settings.compiler == "msvc":
            self.settings.compiler.cppstd = "17"
            
        self.requires("libcheck/0.15.2")
        self.requires("protobuf/6.30.1")
        if self.settings.os == "Windows":
            self.requires("openssl/3.4.1")

    def layout(self):
        cmake_layout(self)

    def configure(self):
        self.options["libcheck"].with_subunit = False

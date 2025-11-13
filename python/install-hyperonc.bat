@echo off
set HYPERONC_URL=https://github.com/DaddyWesker/hyperon-experimental.git
set HYPERONC_REV=release-win

:loop
IF NOT "%1"=="" (
    IF "%1"=="-u" (
        SET HYPERONC_URL=%2
        SHIFT
    )
    IF "%1"=="-r" (
        SET HYPERONC_REV=%2
        SHIFT
    )
    IF "%1"=="?" (
        echo Usage: %~nx0 [-u hyperonc_repo_url] [-r hyperonc_revision]
        echo -u hyperonc_repo_url    Git repo URL to get hyperonc source code
        echo -r hyperonc_revision    Revision of hyperonc to get from Git
        exit /b
    )
    IF "%1"=="-h" (
        echo Usage: %~nx0 [-u hyperonc_repo_url] [-r hyperonc_revision]
        echo -u hyperonc_repo_url    Git repo URL to get hyperonc source code
        echo -r hyperonc_revision    Revision of hyperonc to get from Git
        exit /b
    )
    SHIFT
    GOTO :loop
)

echo hyperonc repository URL: %HYPERONC_URL%
echo hyperonc revision: %HYPERONC_REV%

IF NOT "%RUNNER_TEMP%"=="" set TEMP_FOLDER=%RUNNER_TEMP%
IF "%RUNNER_TEMP%"=="" set TEMP_FOLDER=%USERPROFILE%

set CARGO_HOME=%TEMP_FOLDER%\\.cargo
set RUSTUP_HOME=%TEMP_FOLDER%\\.rustup
curl --proto "=https" --tlsv1.2 -sSf https://static.rust-lang.org/rustup/dist/x86_64-pc-windows-msvc/rustup-init.exe > %TEMP_FOLDER%/rustup-init.exe
call %TEMP_FOLDER%/rustup-init.exe -y
del %TEMP_FOLDER%\rustup-init.exe
set PATH=%PATH%;%TEMP_FOLDER%\\.cargo\\bin
cargo install cbindgen

python -m pip install cmake==3.24 conan==2.19.1 pip==23.1.2
set PATH=%PATH%;%TEMP_FOLDER%\\.local\\bin
conan profile detect --force

rem protobuf-compiler (v3) is required by Das
set PROTOC_ZIP=protoc-31.1-win64.zip
curl -OL https://github.com/protocolbuffers/protobuf/releases/download/v31.1/%PROTOC_ZIP%
mkdir %TEMP_FOLDER%\.local 
tar -xf %PROTOC_ZIP% -C %TEMP_FOLDER%\.local
del -f %PROTOC_ZIP%

mkdir %TEMP_FOLDER%\\hyperonc
cd %TEMP_FOLDER%\\hyperonc
git init
git remote add origin %HYPERONC_URL%
git fetch --depth=1 origin %HYPERONC_REV%
git reset --hard FETCH_HEAD

mkdir %TEMP_FOLDER%\hyperonc\c\build
cd %TEMP_FOLDER%\hyperonc\c\build

rem if this script works on github workflow, it is crucial for cmake to install in the defauls installation folder
rem otherwise it will fail. On the local machine it could be crucial to set install folder manually since it tries
rem to install into C:/ProgramFiles... which could be restricted and script will fail. Because of that cmake args 
rem will or will not contain CMAKE_INSTALL_PREFIX argument. 

set CMAKE_ARGS=-DBUILD_SHARED_LIBS=ON -DCMAKE_CONFIGURATION_TYPES=Release -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=%TEMP_FOLDER%/hyperonc/conan_provider.cmake

IF "%RUNNER_TEMP%"=="" set CMAKE_ARGS=set CMAKE_ARGS=-DBUILD_SHARED_LIBS=ON -DCMAKE_CONFIGURATION_TYPES=Release -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=%TEMP_FOLDER%/hyperonc/conan_provider.cmake -DCMAKE_INSTALL_PREFIX=%TEMP_FOLDER%\\.local

echo hyperonc CMake arguments: %CMAKE_ARGS%
cmake %CMAKE_ARGS% ..
cmake --build . --config Release
cmake --build . --target check --config Release
cmake --build . --target install --config Release
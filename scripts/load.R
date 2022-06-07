# Source all files in /src
for (f in list.files("../src", pattern="*.R")) {
    source(paste0("../src/", f))
}



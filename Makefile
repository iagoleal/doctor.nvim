
SRC = $(wildcard fnl/doctor/*.fnl)
OUT = $(patsubst fnl/doctor/%.fnl,lua/doctor/%.lua,$(SRC))


all: $(OUT)

lua/doctor/%.lua: fnl/doctor/%.fnl
	fennel --compile "$<" > "$@"

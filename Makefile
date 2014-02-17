SRC_DIR = src
EBIN_DIR = ebin

all: compile

compile:
	@mkdir -p $(EBIN_DIR)
	erlc -o $(EBIN_DIR) $(SRC_DIR)/*.erl

clean:
	rm -rf $(EBIN_DIR)/*.beam

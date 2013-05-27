CFLAGS := -fPIC -g -Wall -Werror
CC := gcc
NAME := kernel_routes

all: lib

$(NAME): $(NAME).o
	$(CC) $(CFLAGS) $^ -o $@

lib: lib$(NAME).so

lib$(NAME).so: $(NAME).o
	$(CC) -shared -Wl,-soname,lib$(NAME).so $^ -o $@

clean:
	$(RM) *.o *.so* $(NAME)

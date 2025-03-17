#include "noob.h"

int main(int argc, const char **argv) {
  noob_rebuild_yourself(argc, argv);

	if (noob_help(argc, argv,"usage ./noob: builds klaus compiler", "\t-r <path> to compile and run a .kl file")) exit(0);
	
  noob_run("rm -rf ./src/klaus");
  noob_run("ocamlopt ./src/klaus.ml -o ./src/klaus");

	int i;
  if ((i = noob_has_flag(argc, argv, "-r")) && argc + 1 > i) {
		noob_string *s = noob_string_create_from("./src/klaus ");
    noob_string_append(s, argv[i+1]);
		noob_string_append(s, " && ./src/out");
		noob_run_cmd(s);
  }

  return 0;
}

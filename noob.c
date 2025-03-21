#include "noob.h"

int main(int argc, const char **argv) {
  noob_rebuild_yourself(argc, argv);

	if (noob_help(argc, argv,"usage ./noob: builds klaus compiler",
								"\t-r <path> to compile and run a .kl file",
								"\t-p keep preprocessed file")) exit(0);

	// Preprocessing: replace asm header with actual asm from file
	noob_string *head = noob_file_read("asm/head.S");
	noob_string *src = noob_file_read("src/klaus.ml");
	noob_string_replace(src, "ASMHEADER", head->buf);
	noob_file_write("src/klausp.ml", src->buf);
	noob_string_free(head);
	noob_string_free(src);

	int res = noob_run("ocamlopt ./src/klausp.ml -o ./src/klaus");

	if (!noob_has_flag(argc, argv, "-p") && !res)
			noob_run("rm -rf src/klausp.ml src/klausp.cmi src/klausp.cmx src/klausp.o");
	
	int i;
  if ((i = noob_has_flag(argc, argv, "-r")) && argc + 1 > i) {
		noob_string *s = noob_string_create_from("./src/klaus ");
    noob_string_append(s, argv[i+1]);
		noob_string_append(s, " && ./out");
		noob_run_cmd(s);
  }

  return 0;
}

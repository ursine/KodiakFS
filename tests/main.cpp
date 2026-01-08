
extern "C" {
  #include "ei.h"
}

int main(int argc, char *argv[]) {
  ei_init();

  int i = 0;
  
  ei_x_buff buf;
  ei_x_new(&buf);
  ei_x_format_wo_ver(&buf, "{~a,~i}", "tobbe", 3928);
  ei_print_term(stdout, buf.buff, &i);
  ei_x_free(&buf);

  
  return 0;
}

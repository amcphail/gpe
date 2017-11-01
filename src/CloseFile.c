#include <fitsio.h>

#include "CloseFile.h"

void CloseFile(fitsfile* fptr)
{
  int status;

  ffclos(fptr,&status);
}

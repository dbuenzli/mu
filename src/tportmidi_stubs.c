/*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>
#include <caml/bigarray.h>

#include <portmidi.h>


CAMLprim value ocaml_tpm_get_error_text (value e)
{
  return caml_copy_string (Pm_GetErrorText ((PmError)(Int_val (e))));
}

CAMLprim value ocaml_tpm_get_host_error_text (value unit)
{
  char msg[256] = { 0 };
  Pm_GetHostErrorText (msg, 255); /* Not a very nice interface */
  return caml_copy_string (msg);
}

CAMLprim value ocaml_tpm_initialize (value unit)
{
  return Val_int (Pm_Initialize ());
}

CAMLprim value ocaml_tpm_terminate (value unit)
{
  return Val_int (Pm_Terminate ());
}

CAMLprim value ocaml_tpm_count_devices (value unit)
{
  return Val_int (Pm_CountDevices ());
}

CAMLprim value ocaml_tpm_get_device_info (value di)
{
  CAMLparam1 (di);
  CAMLlocal2 (r, i);

  const PmDeviceInfo* d = Pm_GetDeviceInfo (Int_val (di));

  if (d == NULL)
  {
    r = caml_alloc (1, 1);
    Store_field (r, 0, Val_int (pmInvalidDeviceId) /* make up something */);
  } else {
    r = caml_alloc (1, 0);
    i = caml_alloc (5, 0);
    Store_field (r, 0, i);
    Store_field (i, 0, caml_copy_string (d->interf));
    Store_field (i, 1, caml_copy_string (d->name));
    Store_field (i, 2, Val_bool (d->input));
    Store_field (i, 3, Val_bool (d->output));
    Store_field (i, 4, Val_bool (d->is_virtual));
  }
  CAMLreturn (r);
}

CAMLprim value ocaml_tpm_create_virtual_input (value n, value i)
{
  return Val_int (Pm_CreateVirtualInput (String_val (n), String_val (i), NULL));
}

CAMLprim value ocaml_tpm_create_virtual_output (value n, value i)
{
  return Val_int (Pm_CreateVirtualOutput (String_val (n), String_val (i),
                                          NULL));
}

CAMLprim value ocaml_tpm_delete_virtual_device (value did)
{
  return Val_int (Pm_DeleteVirtualDevice (Int_val (did)));
}

#define PortMidiStream_val(v) (*((PortMidiStream **) Data_abstract_val(v)))

CAMLprim value ocaml_tpm_open_input (value did, value bsize)
{

  CAMLparam2 (did, bsize);
  CAMLlocal2 (ret, s);
  PmError err;
  PortMidiStream *pms;

  err = Pm_OpenInput (&pms, Int_val (did), NULL, Int_val (bsize), NULL, NULL);
  if (err == pmNoError)
  {
    s = caml_alloc (1, Abstract_tag);
    *((PortMidiStream **) Data_abstract_val (s)) = pms;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, s);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_int (err));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_tpm_open_output (value did, value bsize, value lat)
{

  CAMLparam3 (did, bsize, lat);
  CAMLlocal2 (ret, s);
  PmError err;
  PortMidiStream *pms;

  err = Pm_OpenOutput (&pms, Int_val (did), NULL, Int_val (bsize), NULL, NULL,
                       Int_val (lat));
  if (err == pmNoError)
  {
    s = caml_alloc (1, Abstract_tag);
    *((PortMidiStream **) Data_abstract_val (s)) = pms;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, s);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_int (err));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_tpm_close (value sp)
{
  return Val_int (Pm_Close (PortMidiStream_val (sp)));
}

CAMLprim value ocaml_tpm_abort (value sp)
{
  return Val_int (Pm_Abort (PortMidiStream_val (sp)));
}

CAMLprim value ocaml_tpm_synchronize (value sp)
{
  return Val_int (Pm_Synchronize (PortMidiStream_val (sp)));
}

CAMLprim value ocaml_tpm_poll (value sp)
{
  return Val_int (Pm_Poll (PortMidiStream_val (sp)));
}

CAMLprim value ocaml_tpm_read (value sp, value ba, value count)
{
  PmError err;
  PortMidiStream *s = PortMidiStream_val (sp);
  PmEvent *data = (PmEvent *)(Caml_ba_data_val (ba));
  caml_release_runtime_system ();
  err = Pm_Read (s, data, Int_val (count));
  caml_acquire_runtime_system ();
  return Val_int (err);
}

CAMLprim value ocaml_tpm_write (value sp, value ba, value count)
{
  PmError err;
  PortMidiStream *s = PortMidiStream_val (sp);
  PmEvent *data = (PmEvent *)(Caml_ba_data_val (ba));
  caml_release_runtime_system ();
  err = Pm_Write (s, data, Int_val (count));
  caml_acquire_runtime_system ();
  return Val_int (err);
}

CAMLprim value ocaml_tpm_write_short (value sp, value w, value m)
{
  PmError err;
  PortMidiStream *s = PortMidiStream_val (sp);
  err = Pm_WriteShort (s, Int32_val (w), Int32_val (m));
  return Val_int (err);
}

CAMLprim value ocaml_tpm_write_sysex (value sp, value w, value m)
{
  PmError err;
  PortMidiStream *s = PortMidiStream_val (sp);
  /* XXX it would likely be good to copy and release the sys here. */
  err = Pm_WriteSysEx (s, Int32_val (w), (unsigned char *)String_val (m));
  return Val_int (err);
}

/*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/

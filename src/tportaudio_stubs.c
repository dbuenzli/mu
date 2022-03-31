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

#include <portaudio.h>

CAMLprim value ocaml_tpa_version (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (r);

  const PaVersionInfo* v = Pa_GetVersionInfo ();

  r = caml_alloc (5, 0);
  Store_field (r, 0, Val_int (v->versionMajor));
  Store_field (r, 1, Val_int (v->versionMinor));
  Store_field (r, 2, Val_int (v->versionSubMinor));
  Store_field (r, 3, caml_copy_string (v->versionControlRevision));
  Store_field (r, 4, caml_copy_string (v->versionText));
  CAMLreturn (r);
}

CAMLprim value ocaml_tpa_get_error_text (value e)
{
  return caml_copy_string (Pa_GetErrorText ((PaError)(Int_val (e))));
}

CAMLprim value ocaml_tpa_get_host_error_text (value unit)
{
  const PaHostErrorInfo* e = Pa_GetLastHostErrorInfo ();
  if (e == NULL || e->errorText == NULL)
    return caml_copy_string ("Unknown host error text");
  return caml_copy_string (e->errorText);
}

CAMLprim value ocaml_tpa_initialize (value unit)
{
  return Val_int (Pa_Initialize ());
}

CAMLprim value ocaml_tpa_terminate (value unit)
{
  return Val_int (Pa_Terminate ());
}

/* Time */

CAMLprim value ocaml_tpa_sleep (value ms)
{
  Pa_Sleep (Int_val (ms));
  return Val_unit;
}

/* Host APIs */

CAMLprim value ocaml_tpa_get_host_api_count (value unit)
{
  return Val_int (Pa_GetHostApiCount ());
}

CAMLprim value ocaml_tpa_get_default_host_api (value unit)
{
  return Val_int (Pa_GetDefaultHostApi ());
}

CAMLprim value
ocaml_tpa_host_api_device_index_to_device_index (value ai, value di)
{
  return Val_int (Pa_HostApiDeviceIndexToDeviceIndex (Int_val (ai),
                                                      Int_val (di)));
}

CAMLprim value ocaml_mu_ba_ptr (value ba)
{
  return caml_copy_nativeint ((intnat)Caml_ba_data_val (ba));
}

CAMLprim value ocaml_tpa_get_host_api_info (value ai)
{
  CAMLparam1 (ai);
  CAMLlocal2 (r,i);

  const PaHostApiInfo* a = Pa_GetHostApiInfo (Int_val (ai));

  if (a == NULL)
  {
    r = caml_alloc (1, 1);
    Store_field (r, 0, Val_int (paInvalidHostApi) /* make up something */);
  } else {
    r = caml_alloc (1, 0);
    i = caml_alloc (5, 0);
    Store_field (r, 0, i);
    Store_field (i, 0, Val_int (a->type));
    Store_field (i, 1, caml_copy_string (a->name));
    Store_field (i, 2, Val_int (a->deviceCount));
    Store_field (i, 3, Val_int (a->defaultInputDevice));
    Store_field (i, 4, Val_int (a->defaultOutputDevice));
  }
  CAMLreturn (r);
}

/* Devices */

CAMLprim value ocaml_tpa_get_device_count (value unit)
{
  return Val_int (Pa_GetDeviceCount ());
}

CAMLprim value ocaml_tpa_get_default_input_device (value unit)
{
  return Val_int (Pa_GetDefaultInputDevice ());
}

CAMLprim value ocaml_tpa_get_default_output_device (value unit)
{
  return Val_int (Pa_GetDefaultOutputDevice ());
}

CAMLprim value ocaml_tpa_get_device_info (value di)
{
  CAMLparam1 (di);
  CAMLlocal2 (r, i);

  const PaDeviceInfo* d = Pa_GetDeviceInfo (Int_val (di));

  if (d == NULL)
  {
    r = caml_alloc (1, 1);
    Store_field (r, 0, Val_int (paInvalidDevice) /* make up something */);
  } else {
    r = caml_alloc (1, 0);
    i = caml_alloc (9, 0);
    Store_field (r, 0, i);
    Store_field (i, 0, caml_copy_string (d->name));
    Store_field (i, 1, Val_int (d->hostApi));
    Store_field (i, 2, Val_int (d->maxInputChannels));
    Store_field (i, 3, Val_int (d->maxOutputChannels));
    Store_field (i, 4, caml_copy_double (d->defaultLowInputLatency));
    Store_field (i, 5, caml_copy_double (d->defaultLowOutputLatency));
    Store_field (i, 6, caml_copy_double (d->defaultHighInputLatency));
    Store_field (i, 7, caml_copy_double (d->defaultHighOutputLatency));
    Store_field (i, 8, caml_copy_double (d->defaultSampleRate));
  }
  CAMLreturn (r);
}

/* Stream parameters */

static void ocaml_mu_fill_stream_parameters
(value i, PaStreamParameters *p)
{
  p->device = Int_val (Field(i, 0));
  p->channelCount = Int_val (Field (i, 1));
  p->sampleFormat = Int32_val (Field (i, 2));
  p->suggestedLatency = Double_val (Field (i, 3));
  p->hostApiSpecificStreamInfo = NULL;
}

CAMLprim value ocaml_tpa_is_format_supported
(value i_opt, value o_opt, value sr)
{
  PaStreamParameters ip;
  PaStreamParameters op;
  if (Is_some (i_opt)) ocaml_mu_fill_stream_parameters (Field (i_opt, 0), &ip);
  if (Is_some (o_opt)) ocaml_mu_fill_stream_parameters (Field (o_opt, 0), &op);
  return (Val_int (Pa_IsFormatSupported ((Is_some (i_opt) ? &ip : NULL),
                                         (Is_some (o_opt) ? &op : NULL),
                                         Double_val (sr))));
}

/* Streams */

#define PaStream_val(v) (*((PaStream **) Data_abstract_val(v)))

CAMLprim value ocaml_tpa_open_stream
(value i_opt, value o_opt, value sr, value fpb, value flags)
{
  CAMLparam5 (i_opt, o_opt, sr, fpb, flags);
  CAMLlocal2 (ret, s);
  PaError err;
  PaStream *pas;
  PaStreamParameters ip;
  PaStreamParameters op;
  if (Is_some (i_opt)) ocaml_mu_fill_stream_parameters (Field (i_opt, 0), &ip);
  if (Is_some (o_opt)) ocaml_mu_fill_stream_parameters (Field (o_opt, 0), &op);

  err = Pa_OpenStream (&pas,
                       (Is_some (i_opt) ? &ip : NULL),
                       (Is_some (o_opt) ? &op : NULL),
                       Double_val (sr), Int_val (fpb), Int_val (flags),
                       NULL, NULL);

  if (err == paNoError)
  {
    s = caml_alloc (1, Abstract_tag);
    *((PaStream **) Data_abstract_val (s)) = pas;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, s);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_int (err));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_tpa_open_default_stream
(value ic, value oc, value sf, value sr, value fpb)
{
  CAMLparam5(ic, oc, sf, sr, fpb);
  CAMLlocal2 (ret, s);
  PaError err;
  PaStream *pas;
  err = Pa_OpenDefaultStream (&pas, Int_val (ic), Int_val (oc), Int32_val (sf),
                              Double_val (sr), Int_val (fpb), NULL, NULL);

  if (err == paNoError)
  {
    s = caml_alloc (1, Abstract_tag);
    *((PaStream **) Data_abstract_val (s)) = pas;
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, s);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_int (err));
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_tpa_get_stream_info (value sp)
{
  CAMLparam1 (sp);
  CAMLlocal2 (ret, t);
  const PaStreamInfo *si;
  si = Pa_GetStreamInfo (PaStream_val (sp));
  if (si != NULL) {
    t = caml_alloc (3, 0);
    Store_field (t, 0, caml_copy_double (si->inputLatency));
    Store_field (t, 1, caml_copy_double (si->outputLatency));
    Store_field (t, 2, caml_copy_double (si->sampleRate));
    ret = caml_alloc (1, 0);
    Store_field (ret, 0, t);
  } else {
    ret = caml_alloc (1, 1);
    Store_field (ret, 0, Val_int (paBadStreamPtr) /* makeup something */);
  }
  CAMLreturn (ret);
}

CAMLprim value ocaml_tpa_start_stream (value sp)
{
  return Val_int (Pa_StartStream (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_stop_stream (value sp)
{
  return Val_int (Pa_StopStream (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_abort_stream (value sp)
{
  return Val_int (Pa_AbortStream (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_close_stream (value sp)
{
  return Val_int (Pa_CloseStream (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_is_stream_active (value sp)
{
  return Val_int (Pa_IsStreamActive (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_is_stream_stopped (value sp)
{
  return Val_int (Pa_IsStreamStopped (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_get_stream_time (value sp)
{
  return caml_copy_double (Pa_GetStreamTime (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_get_stream_read_available (value sp)
{
  return Val_int (Pa_GetStreamReadAvailable (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_get_stream_write_available (value sp)
{
  return Val_int (Pa_GetStreamWriteAvailable (PaStream_val (sp)));
}

CAMLprim value ocaml_tpa_write_stream (value sp, value ba, value count)
{
  PaError err;
  PaStream *s = PaStream_val (sp);
  void *data = (void *)(Caml_ba_data_val (ba));
  caml_release_runtime_system ();
  err = Pa_WriteStream (s, data, Int_val (count));
  caml_acquire_runtime_system ();
  return Val_int (err);
}

CAMLprim value ocaml_tpa_read_stream (value sp, value ba, value count)
{
  PaError err;
  PaStream *s = PaStream_val (sp);
  void *data = (void *)(Caml_ba_data_val (ba));
  caml_release_runtime_system ();
  err = Pa_ReadStream (s, data, Int_val (count));
  caml_acquire_runtime_system ();
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

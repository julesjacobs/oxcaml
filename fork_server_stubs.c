/* SCM_RIGHTS fd-passing + peer-cred stubs for the in-driver warm fork-server
 * (OCAMLOPT_FORKSERVER). OCaml's stdlib Unix has no sendmsg/recvmsg with
 * ancillary data or SO_PEERCRED, so we provide them here. Unlike PR #47's
 * standalone (where the client was a C shim), here BOTH ends are OCaml: the
 * client (the ocamlopt process) sends its stdio fds, the daemon receives them.
 * fds are raw ints (Unix.file_descr is an int on Unix). */
#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>

#define MAX_FDS 8

/* forksrv_recvmsg_fds : file_descr -> bytes -> (int * int array)
 * one recvmsg into buf; returns (#bytes, received fds). */
CAMLprim value forksrv_recvmsg_fds(value v_fd, value v_buf)
{
  CAMLparam2(v_fd, v_buf);
  CAMLlocal2(res, fds);
  int fd = Int_val(v_fd);
  struct msghdr msg;
  struct iovec iov;
  char cbuf[CMSG_SPACE(sizeof(int) * MAX_FDS)];
  ssize_t n;

  memset(&msg, 0, sizeof msg);
  iov.iov_base = Bytes_val(v_buf);
  iov.iov_len = caml_string_length(v_buf);
  msg.msg_iov = &iov;
  msg.msg_iovlen = 1;
  msg.msg_control = cbuf;
  msg.msg_controllen = sizeof cbuf;

  do { n = recvmsg(fd, &msg, 0); } while (n < 0 && errno == EINTR);
  if (n < 0) uerror("recvmsg", Nothing);

  int got[MAX_FDS];
  int nfds = 0;
  struct cmsghdr *cm;
  for (cm = CMSG_FIRSTHDR(&msg); cm != NULL; cm = CMSG_NXTHDR(&msg, cm)) {
    if (cm->cmsg_level == SOL_SOCKET && cm->cmsg_type == SCM_RIGHTS) {
      int cnt = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);
      int *fdp = (int *)CMSG_DATA(cm);
      for (int i = 0; i < cnt && nfds < MAX_FDS; i++) got[nfds++] = fdp[i];
    }
  }
  fds = caml_alloc(nfds, 0);
  for (int i = 0; i < nfds; i++) Field(fds, i) = Val_int(got[i]);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_long(n));
  Store_field(res, 1, fds);
  CAMLreturn(res);
}

/* forksrv_sendmsg_fds : file_descr -> bytes -> int -> int array -> int
 * one sendmsg of buf[0..len) with the given fds as SCM_RIGHTS ancillary. */
CAMLprim value forksrv_sendmsg_fds(value v_fd, value v_buf, value v_len,
                                   value v_fds)
{
  CAMLparam4(v_fd, v_buf, v_len, v_fds);
  int fd = Int_val(v_fd);
  int nfds = Wosize_val(v_fds);
  if (nfds > MAX_FDS) nfds = MAX_FDS;
  struct msghdr msg;
  struct iovec iov;
  char cbuf[CMSG_SPACE(sizeof(int) * MAX_FDS)];
  ssize_t n;

  memset(&msg, 0, sizeof msg);
  memset(cbuf, 0, sizeof cbuf);
  iov.iov_base = Bytes_val(v_buf);
  iov.iov_len = Int_val(v_len);
  msg.msg_iov = &iov;
  msg.msg_iovlen = 1;
  if (nfds > 0) {
    msg.msg_control = cbuf;
    msg.msg_controllen = CMSG_SPACE(sizeof(int) * nfds);
    struct cmsghdr *cm = CMSG_FIRSTHDR(&msg);
    cm->cmsg_level = SOL_SOCKET;
    cm->cmsg_type = SCM_RIGHTS;
    cm->cmsg_len = CMSG_LEN(sizeof(int) * nfds);
    int *fdp = (int *)CMSG_DATA(cm);
    for (int i = 0; i < nfds; i++) fdp[i] = Int_val(Field(v_fds, i));
  }
  do { n = sendmsg(fd, &msg, 0); } while (n < 0 && errno == EINTR);
  if (n < 0) uerror("sendmsg", Nothing);
  CAMLreturn(Val_long(n));
}

/* forksrv_clearenv : unit -> unit */
CAMLprim value forksrv_clearenv(value unit)
{
  CAMLparam1(unit);
  clearenv();
  CAMLreturn(Val_unit);
}

/* forksrv_peer_uid : file_descr -> int   (SO_PEERCRED uid, or -1) */
CAMLprim value forksrv_peer_uid(value v_fd)
{
  CAMLparam1(v_fd);
  struct ucred cr;
  socklen_t len = sizeof cr;
  if (getsockopt(Int_val(v_fd), SOL_SOCKET, SO_PEERCRED, &cr, &len) < 0)
    CAMLreturn(Val_int(-1));
  CAMLreturn(Val_int((int)cr.uid));
}

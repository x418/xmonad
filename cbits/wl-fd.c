#define _GNU_SOURCE
/* Ancillary-data send/receive for the Wayland socket.
 *
 * Wayland passes file descriptors as SCM_RIGHTS ancillary data attached to the
 * same sendmsg() that carries the request bytes; the receiver correlates them
 * by order.  Two things rule out doing this from Haskell alone:
 *
 *   - network's sendMsg and sendBufMsg both take a mandatory SockAddr and
 *     always set msg_name, which Linux rejects with EISCONN on a connected
 *     socket.  There is no variant that omits it.  (network's sendFd/recvFd
 *     use their own single-byte protocol, which is not Wayland's.)
 *
 *   - CMSG_SPACE, CMSG_LEN, CMSG_FIRSTHDR and CMSG_DATA are macros whose
 *     alignment rules are platform-defined.  Reimplementing them against
 *     hard-coded struct offsets is the kind of thing that works on the machine
 *     it was written on.
 *
 * So the msghdr juggling happens here, where the macros exist, and Haskell
 * sees two ordinary functions.  This is the only C in the package, and it
 * exists only under -f river.
 */

#include <errno.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

/* Send buf[0..len) with nfds descriptors attached as SCM_RIGHTS.
 * Returns bytes sent, or -1 with errno set. */
ssize_t
hs_wl_sendmsg_fds(int sock, const void *buf, size_t len,
                  const int *fds, size_t nfds)
{
    struct msghdr msg;
    struct iovec iov;
    /* Enough for any plausible batch; Wayland sends at most a handful. */
    char control[CMSG_SPACE(sizeof(int) * 16)];

    if (nfds > 16) {
        errno = EINVAL;
        return -1;
    }

    iov.iov_base = (void *) buf;
    iov.iov_len = len;

    memset(&msg, 0, sizeof msg);
    msg.msg_name = NULL;        /* connected socket: must stay NULL */
    msg.msg_namelen = 0;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;

    if (nfds > 0) {
        struct cmsghdr *cmsg;
        memset(control, 0, sizeof control);
        msg.msg_control = control;
        msg.msg_controllen = CMSG_SPACE(sizeof(int) * nfds);

        cmsg = CMSG_FIRSTHDR(&msg);
        cmsg->cmsg_level = SOL_SOCKET;
        cmsg->cmsg_type = SCM_RIGHTS;
        cmsg->cmsg_len = CMSG_LEN(sizeof(int) * nfds);
        memcpy(CMSG_DATA(cmsg), fds, sizeof(int) * nfds);
    } else {
        msg.msg_control = NULL;
        msg.msg_controllen = 0;
    }

    return sendmsg(sock, &msg, MSG_NOSIGNAL);
}

/* Receive up to len bytes into buf, collecting any descriptors into fds_out
 * (capacity nfds_cap).  *nfds_out receives how many were written.
 *
 * Descriptors arrive with CLOEXEC set: a window manager forks constantly --
 * every spawn -- and a buffer fd leaking into a child would keep the shared
 * mapping alive past its owner.
 *
 * Returns bytes received, or -1 with errno set. */
ssize_t
hs_wl_recvmsg_fds(int sock, void *buf, size_t len,
                  int *fds_out, size_t nfds_cap, size_t *nfds_out)
{
    struct msghdr msg;
    struct iovec iov;
    struct cmsghdr *cmsg;
    char control[CMSG_SPACE(sizeof(int) * 16)];
    ssize_t n;

    *nfds_out = 0;

    iov.iov_base = buf;
    iov.iov_len = len;

    memset(&msg, 0, sizeof msg);
    memset(control, 0, sizeof control);
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = control;
    msg.msg_controllen = sizeof control;

    n = recvmsg(sock, &msg, MSG_CMSG_CLOEXEC);
    if (n < 0)
        return n;

    for (cmsg = CMSG_FIRSTHDR(&msg); cmsg != NULL;
         cmsg = CMSG_NXTHDR(&msg, cmsg)) {
        if (cmsg->cmsg_level == SOL_SOCKET && cmsg->cmsg_type == SCM_RIGHTS) {
            size_t payload = cmsg->cmsg_len - CMSG_LEN(0);
            size_t count = payload / sizeof(int);
            size_t i;
            for (i = 0; i < count && *nfds_out < nfds_cap; i++) {
                memcpy(&fds_out[*nfds_out],
                       CMSG_DATA(cmsg) + i * sizeof(int),
                       sizeof(int));
                (*nfds_out)++;
            }
        }
    }

    return n;
}

/* --- shared memory for wl_shm buffers ------------------------------------
 *
 * A wl_buffer's pixels live in memory shared with the compositor, which means
 * an anonymous file both processes map.  Neither memfd_create nor mmap is
 * exposed by the unix package, and both are a couple of lines here, so they
 * keep the fd machinery company rather than pulling in another dependency.
 */

#include <sys/mman.h>
#include <unistd.h>

/* An anonymous, sealed-capable file of the given size, CLOEXEC as with
 * received descriptors: the window manager forks on every spawn.
 * Returns the fd, or -1 with errno set. */
int
hs_wl_memfd(const char *name, size_t size)
{
    int fd = memfd_create(name, MFD_CLOEXEC | MFD_ALLOW_SEALING);
    if (fd < 0)
        return -1;
    if (ftruncate(fd, (off_t) size) < 0) {
        int saved = errno;
        close(fd);
        errno = saved;
        return -1;
    }
    return fd;
}

/* Map size bytes of fd read/write shared.  Returns NULL on failure. */
void *
hs_wl_mmap(int fd, size_t size)
{
    void *p = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    return p == MAP_FAILED ? NULL : p;
}

int
hs_wl_munmap(void *addr, size_t size)
{
    return munmap(addr, size);
}

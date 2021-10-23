#ifndef BR_FILE_H
#define BR_FILE_H

#ifdef BR_PLATFORM_LINUX
extern "C" {
	#include <sys/mman.h>
	#include <sys/types.h>
	#include <sys/stat.h>
	#include <fcntl.h>
	#include <unistd.h>
}
#endif

#include <lib/def.hpp>
#include <lib/str.hpp>
#include <lib/mem.hpp>

namespace br {

	inline void unmap_file(str_view file) {
#ifdef BR_PLATFORM_LINUX
		i32_t status = munmap((void*)file.begin, length(file));
		BR_ASSERT(status != -1);
		(void)status;
#else
		(void)status;
		BR_UNIMPLEMENTED();
#endif
	}

	// Map a file and return a string view.
	inline str_view map_file(str_view path) {
#ifdef BR_PLATFORM_LINUX
		i32_t status;

		// Copy path name into buffer and add null terminator.
		char buf[PATH_MAXIMUM_LENGTH];
		BR_ASSERT(length(path) < PATH_MAXIMUM_LENGTH);

		// Get current path.
		auto pptr = getcwd(buf, PATH_MAXIMUM_LENGTH);
		BR_ASSERT(pptr != nullptr);

		// Get size of current path.
		size_t written = length((const char*)buf);
		buf[written] = '/'; // Add `/` to end of current path.

		// Copy user provided path into buffer.
		memcpy(path.begin, buf + written + 1, length(path));
		buf[written + length(path) + 1] = '\0';

		// Open file.
		fd_t fd = open(buf, O_RDONLY, 0);
		BR_ASSERT(fd != -1);

		// Get size of file.
		struct stat info;
		status = fstat(fd, &info);
		BR_ASSERT(status != -1);
		size_t sz = info.st_size;

		// Memory map the file.
		auto mptr = static_cast<char*>(mmap(nullptr, sz, PROT_READ, MAP_PRIVATE | MAP_NORESERVE | MAP_POPULATE, fd, 0));
		BR_ASSERT(mptr != MAP_FAILED);

		// Hint memory access pattern.
		status = madvise(mptr, sz, MADV_SEQUENTIAL | MADV_WILLNEED);
		BR_ASSERT(status != -1);

		// Close file.
		status = close(fd);
		BR_ASSERT(status != -1);

		(void)status;
		(void)pptr;

		return make_sv(mptr, sz);

#else
		(void)status;
		(void)pptr;

		BR_UNIMPLEMENTED();
		return {};
#endif
	}

}

#endif


#undef OSD_OS_VMS      /* operating system VMS       */
#undef OSD_OS_UNIX     /* operating system UNIX      */
#undef OSD_OS_DOS      /* operating system DOS       */
#undef OSD_OS_LINUX    /* operating system LINUX     */

#undef OSD_ARCH_VMS
#undef OSD_ARCH_UNIX
#undef OSD_ARCH_DOS
#undef OSD_ARCH_LINUX
#undef OSD_ARCH_SUN4SOL2

#if   (defined(LF90))  /* LF90 has to be tested first, fpp from an other OS can be used */
#define OSD_ARCH_DOS
#define OSD_OS_DOS

#elif (defined(__sparc))
#define OSD_ARCH_SUN4SOL2
#define OSD_OS_UNIX

#elif (defined(linux))
#define OSD_ARCH_LINUX
#define OSD_OS_LINUX

#elif (defined(__linux))
#define OSD_ARCH_LINUX
#define OSD_OS_LINUX

#elif (defined(__linux__))
#define OSD_ARCH_LINUX
#define OSD_OS_LINUX

#elif (defined(__WINNT) || defined(__WIN32))  /* dos gnu */
#define OSD_ARCH_DOS
#define OSD_OS_DOS

#elif (defined(windows))                      /* dos g95 */
#define OSD_ARCH_DOS
#define OSD_OS_DOS

#elif (defined(CVF))                          /* Compaq Visual Fortran */
#define OSD_ARCH_DOS
#define OSD_OS_DOS

#elif (defined(IFORT))                        /* Intel Fortran */
#define OSD_ARCH_DOS
#define OSD_OS_DOS

#else
c #error ************************ unknown architecture ************************
      ERROR, unknown architecture
#endif



#undef OSD_CMP_IFORT      /* DOS and LINUX (neowulf) Intel Fortran compiler */
#undef OSD_CMP_CVF        /* DOS Compaq Digital Fortran compiler            */
#undef OSD_CMP_LF90       /* DOS Lahey Fortran 90                           */
#undef OSD_CMP_GNU        /* GNU compiler (all OS)                          */
#undef OSD_CMP_PGF        /* Portland Group Linux compiler                  */
#undef OSD_CMP_IFC        /* Intel Fortran Compiler Linux                   */
#undef OSD_CMP_SUN        /* SUN compiler (UNIX)                            */

#define OSD_CMP_VERSION 0 /* compiler version, sometimes known, sometimes usefull */


#if   (defined(IFORT))
#define OSD_CMP_IFORT     /* DOS Intel Fortran compiler */

#elif (defined(__INTEL_COMPILER))
#define OSD_CMP_IFORT     /* LINUX (neowulf) Intel Fortran compiler */

#elif (defined(CVF))
#define OSD_CMP_CVF       /* DOS Compaq Digital Fortran compiler */

#elif (defined(LF90))
#define OSD_CMP_LF90      /* DOS Lahey Fortran 90          */

#elif (defined(PGF) || defined(__PGI))
#define OSD_CMP_PGF         /* Portland Group Linux compiler */

#if   (defined(PGF_VRS_4))  /* define on commandline with -DPGF_VRS_4 */
#define OSD_CMP_VERSION 4   /* version 4 old version Fortran90  */
#elif (defined(PGF_VRS_9))  /* define on commandline with -DPGF_VRS_9 */
#define OSD_CMP_VERSION 9   /* version 9 is Fortran95  */
#else
     ERROR, VERSION OF PORTLAND GROUP COMPILER NOT KNOWN
#endif

#elif (defined(IFC))
#define OSD_CMP_IFC       /* Intel Fortran Compiler Linux */

#elif (defined(__GNUC__))
#define OSD_CMP_GNU       /* GNU compiler (all OS)         */

#elif (defined(sun))
c the predifined symbol 'sun' is always available on Solaris OS
c so other compiler symbols which can be used on Solaris, like __GNUC__, should
c be used before this one
#define OSD_CMP_SUN       /* SUN compiler (UNIX)           */
#else
c #error  ************************* unknown compiler *************************
      ERROR, unknown compiler
#endif




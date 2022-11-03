module metis_enum
  ! metis 5.1.0
  implicit none

  integer, parameter :: METIS_NOPTIONS = 40
  
  !/*------------------------------------------------------------------------
  !* Enum type definitions
  !*-------------------------------------------------------------------------*/
  !/*! Return codes */
  !typedef enum {
  integer, parameter :: METIS_OK           =  1 !/*!< Returned normally */
  integer, parameter :: METIS_ERROR_INPUT  = -2 !/*!< Returned due to erroneous inputs and/or options */
  integer, parameter :: METIS_ERROR_MEMORY = -3 !/*!< Returned due to insufficient memory */
  integer, parameter :: METIS_ERROR        = -4 !/*!< Some other errors */
  !} rstatus_et;

  !/*! Operation type codes */
  !typedef enum {
  integer, parameter :: METIS_OP_PMETIS = 0
  integer, parameter :: METIS_OP_KMETIS = 1
  integer, parameter :: METIS_OP_OMETIS = 2
  !} moptype_et;

  !/*! Options codes (i.e., options[]) */
  !typedef enum {
  integer, parameter :: METIS_OPTION_PTYPE     =  0+1
  integer, parameter :: METIS_OPTION_OBJTYPE   =  1+1
  integer, parameter :: METIS_OPTION_CTYPE     =  2+1
  integer, parameter :: METIS_OPTION_IPTYPE    =  3+1
  integer, parameter :: METIS_OPTION_RTYPE     =  4+1
  integer, parameter :: METIS_OPTION_DBGLVL    =  5+1
  integer, parameter :: METIS_OPTION_NITER     =  6+1
  integer, parameter :: METIS_OPTION_NCUTS     =  7+1
  integer, parameter :: METIS_OPTION_SEED      =  8+1
  integer, parameter :: METIS_OPTION_NO2HOP    =  9+1
  integer, parameter :: METIS_OPTION_MINCONN   = 10+1
  integer, parameter :: METIS_OPTION_CONTIG    = 11+1
  integer, parameter :: METIS_OPTION_COMPRESS  = 12+1
  integer, parameter :: METIS_OPTION_CCORDER   = 13+1
  integer, parameter :: METIS_OPTION_PFACTOR   = 14+1
  integer, parameter :: METIS_OPTION_NSEPS     = 15+1
  integer, parameter :: METIS_OPTION_UFACTOR   = 16+1
  integer, parameter :: METIS_OPTION_NUMBERING = 17+1

  !/* Used for command-line parameter purposes */
  integer, parameter :: METIS_OPTION_HELP      = 18+1
  integer, parameter :: METIS_OPTION_TPWGTS    = 19+1
  integer, parameter :: METIS_OPTION_NCOMMON   = 20+1
  integer, parameter :: METIS_OPTION_NOOUTPUT  = 21+1
  integer, parameter :: METIS_OPTION_BALANCE   = 22+1
  integer, parameter :: METIS_OPTION_GTYPE     = 23+1
  integer, parameter :: METIS_OPTION_UBVEC     = 24+1
  !} moptions_et;

  !/*! Partitioning Schemes */
  !typedef enum {
  integer, parameter :: METIS_PTYPE_RB   = 0
  integer, parameter :: METIS_PTYPE_KWAY = 1
  !} mptype_et;

  !/*! Graph types for meshes */
  !typedef enum {
  integer, parameter :: METIS_GTYPE_DUAL  = 0
  integer, parameter :: METIS_GTYPE_NODAL = 1
  !} mgtype_et;

  ! /*! Coarsening Schemes */
  !typedef enum {
  integer, parameter :: METIS_CTYPE_RM   = 0 
  integer, parameter :: METIS_CTYPE_SHEM = 1
  !} mctype_et;

  !/*! Initial partitioning schemes */
  !typedef enum {
  integer, parameter :: METIS_IPTYPE_GROW    = 0
  integer, parameter :: METIS_IPTYPE_RANDOM  = 1
  integer, parameter :: METIS_IPTYPE_EDGE    = 2
  integer, parameter :: METIS_IPTYPE_NODE    = 3
  integer, parameter :: METIS_IPTYPE_METISRB = 4
  !} miptype_et;

  !/*! Refinement schemes */
  !typedef enum {
  integer, parameter :: METIS_RTYPE_FM        = 0
  integer, parameter :: METIS_RTYPE_GREEDY    = 1
  integer, parameter :: METIS_RTYPE_SEP2SIDED = 2
  integer, parameter :: METIS_RTYPE_SEP1SIDED = 3
  !} mrtype_et;

  !/*! Debug Levels */
  !typedef enum {
  integer, parameter :: METIS_DBG_INFO       = 1    !/*!< Shows various diagnostic messages */
  integer, parameter :: METIS_DBG_TIME       = 2    !/*!< Perform timing analysis */
  integer, parameter :: METIS_DBG_COARSEN    = 4    !/*!< Show the coarsening progress */
  integer, parameter :: METIS_DBG_REFINE     = 8    !/*!< Show the refinement progress */
  integer, parameter :: METIS_DBG_IPART      = 16   !/*!< Show info on initial partitioning */
  integer, parameter :: METIS_DBG_MOVEINFO   = 32   !/*!< Show info on vertex moves during refinement */
  integer, parameter :: METIS_DBG_SEPINFO    = 64   !/*!< Show info on vertex moves during sep refinement */
  integer, parameter :: METIS_DBG_CONNINFO   = 128  !/*!< Show info on minimization of subdomain connectivity */
  integer, parameter :: METIS_DBG_CONTIGINFO = 256  !/*!< Show info on elimination of connected components */
  integer, parameter :: METIS_DBG_MEMORY     = 2048 !/*!< Show info related to wspace allocation */
  !} mdbglvl_et;

  !/* Types of objectives */
  !typedef enum {
  integer, parameter :: METIS_OBJTYPE_CUT  = 0
  integer, parameter :: METIS_OBJTYPE_VOL  = 1
  integer, parameter :: METIS_OBJTYPE_NODE = 2
  !} mobjtype_et;

end module metis_enum



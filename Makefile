BASIS_FILE=gamma.mlb
COMPILER=mlton
COMP_FLAGS=-codegen native
all:
		sh setupBuild.sh
		${COMPILER} ${COMP_FLAGS} ${BASIS_FILE}

build:
	stack build
run: build
	stack exec -- procedural-city-exe ${ARGS}

render:
	processing-java --sketch='./renderer/' --run ${ARGS}

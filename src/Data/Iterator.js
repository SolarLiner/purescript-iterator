const { Tuple } = require("../Data.Tuple");

function applyGen(gen, done=false) {
	const g = gen();
	g.isDone = done;
	return g;
}

exports.iterate = iter => seed => {
	function* gen() {
		while (true) {
			yield (seed = iter(seed));
		}
	}
	return applyGen(gen);
}

exports.forEach = effect => generator => outer => {
	for (const val of generator) {
		const eff = effect(val)(outer);
	}
	return {};
}

exports.empty = applyGen(function* empty() { }, true);

exports.singleton = value => applyGen(function* singleton() { yield value; });

exports.zip = i1 => i2 => applyGen(function* zip() {
	let done = false;
	while (!done) {
		const r1 = i1.next();
		const r2 = i2.next();
		done = r1.done || r2.done;
		yield new Tuple(r1, r2);
	}
});

exports["concat'"] = gens => applyGen(function* concat() {
	for (const gen of gens) {
		yield* gen;
	}
});

exports.concat = exports["concat'"];

exports.range = ({ eq }) => succ => lo => hi => applyGen(function* range() {
	let val = lo;
	while (!eq(val)(hi)) {
		yield val
		val = succ(val);
	}
});

exports.take = n => gen => this.mapImpl(t => t.value1)(this.zip(this.range({ eq: x => y => x === y })(x => x + 1)(0)(n), gen));

exports.fromArray = array => applyGen(function* fromArray() {
	for (const val of array) {
		yield val;
	}
});

exports.toArray = generator => Array.from(generator);

/**
 * @param {Generator} generator
 */
exports.isDone = generator => generator.isDone;


exports.nextImpl = generator => {
	const { value, done } = generator.next();
	generator.isDone = done;
	return new Tuple(value, done);
}

exports.mapImpl = f => generator => {
	function* other() {
		for (const val of generator) {
			yield f(val);
		}
	}
	return applyGen(other);
}

exports.appendImpl = i1 => i2 => {
	function* chain() {
		for (const val of i1) yield val;
		for (const val of i2) yield val;
	}
	return applyGen(chain);
}

exports.foldlImpl = reducer => seed => generator => {
	let result = seed;
	for (const val of generator) {
		result = reducer(result)(val);
	}
	return result;
}

exports.foldrImpl = reducer => seed => generator => {
	let result = seed;
	for (const val of generator) {
		result = reducer(val)(result);
	}
	return result;
}

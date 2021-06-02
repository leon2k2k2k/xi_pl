export const console_output = Promise.resolve(async (x) => {
	console.log(x)
	return Promise.resolve(5);
})

export const console_input = async (x) => Promise.resolve(5)

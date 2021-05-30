console.log(await Promise.resolve(Promise.resolve(1)))

let var_0 = Promise.resolve(1)
console.log(var_0)
console.log(await var_0)
console.log(var_0)

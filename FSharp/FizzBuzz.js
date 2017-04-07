const createRule = matcher => s => n => matcher(n) ? { status: true, value: s(n) } : { status: false };

const multipleOf = n => s => createRule(x => x % n === 0)( _ => s);

const fizz = multipleOf(3)("Fizz");

const buzz = multipleOf(5)("Buzz");

const composeRules = rules => n => rules.reduce((acc, r) => {
    const { value, status } = r(n);
    return acc.status ? { status: status, value: acc.value + value } : acc;
}, { status: true, value: '' });

const fizzBuzz = composeRules([fizz, buzz]);

const applyRules = rules => defaultRule => n =>
    (rules.find(r => r(n).status) || defaultRule)(n);

const defaultRule = createRule(_ => true)(n => n.toString());

const gameEngine = rules => n => {
    const appliedRules = applyRules(rules.regularRules)(rules.defaultRule);
    Array.from({ length: n }, (_, i) => i + 1)
         .map(n => appliedRules(n).value)
         .forEach(n => console.log(n));
};

const lastDigitIs4 = createRule(n => n % 10 === 4)(n => Math.floor(n / 10).toString() + '(4)');

const rules = {
    regularRules: [ lastDigitIs4, fizzBuzz, fizz, buzz ],
    defaultRule: defaultRule
};
# {#guide-testing} How should I test a DApp?

When you are writing a DApp in Reach, you should test it thoroughly.

You may think that because Reach has a verification engine you don't need to write tests.
This is incorrect:
The general verification checks that Reach performs ensure that your program does not do obviously bad things, like violate the linearity of tokens, but it does not check that you wrote the correct program.
Furthermore, the specific verification checks that Reach allows you to write cannot specify every bad thing you want to avoid or every good thing that you want to happen.
Therefore, you need to use a variety of quality assurance mechanisms.

Your general philosophy for thinking about testing is that you write the program multiple times and make sure that each time you write the program, you write the same program:
1. You write the program in Reach.
2. You write the program in Reach's `{!rsh} assert` and `{!rsh} invariant` forms.
3. You write the program with an automated JavaScript test suite.
4. You write the program with an interactive user interface.
5. You write the program with an automated test suite over that user interface.

The connection between any of these two versions of the program is good if whenever you change one, the other one breaks and detects the change.
For example, if you change the program (#1) to use a non-network token, rather than the network token, then your test suite (#3) should reflect this, because your testing accounts will need to get a balance of this non-network token.

Reach provides tools for writing #1, #2, and #3.
You don't have to use our tool for #3 (`{!js} test`) and could use any testing framework you wanted.
We don't provide tools for #4 or #5 and you can use any user interface toolkit or testing regime; we like to use React for #4 and Cypress for #5.

At the micro-level, your testing outlook should be that tests either:
1. Establish that something bad does not happen. Or,
2. Establish that something good does happen.

In order to build a large number of these tests succinctly, you should build a simple "library" that can succinctly initialize your DApp and then run a scenario.
Like this:
```js
const runScenario = async (run) => {
  // Set up the DApp
  const tok = ....;
  const ctc = ....;
  // Deploy it
  await .... ctc.p.Deploy ...
  const ctcInfo = await ctc.getInfo();
  // Run a particular test
  await go({
    ctcInfo,
  });
};
```

Then, you can write lots of different scenarios:
```js
await runScenario(async ({ctcInfo}) => {
  // Test 1
});

await runScenario(async ({ctcInfo}) => {
  // Test 2
});
```

The `{!js} test.one` function is designed for being used in contexts like this.
Then you can use `{!js} test.run` and command-line arguments to select a particular test to run while developing.

Finally, you should use "standard" software testing and quality assurance practices, like continuous integration.
Wikipedia has a great article on [Software Testing](https://en.wikipedia.org/wiki/Software_testing) that is a good place to get started finding more resources.

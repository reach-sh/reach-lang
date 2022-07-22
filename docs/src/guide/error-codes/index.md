# Reading Error Codes

Error codes give insight into the issues in code.
They include a file name, code line reference, and error code designation to identify the erroring code.
This information is used to debug the issue.

To use error codes:

1. Check the file name in the log.
  
![File name](/guide/error-codes/file_name.png)
  
2. Locate the line and character number listed.

![Line and character reference](/guide/error-codes/line_reference.png)

3. Check the error code and message against the line number.

  :::note
  Sometimes, the line number listed is the start of the code block, such as `{!rsh} parallelReduce`, `{!rsh} while`, or `{!rsh} fork`, that has the error, not the line of the code with the error.
  :::

4. Click the link and read the error code information on the [Error Code](##ref-error-codes) page.

![Error code link](/guide/error-codes/error_link.png)

5. Search the code or code block for the issue listed in the description.
6. If you are still having difficulty, request help from [GitHub Discussions](https://github.com/reach-sh/reach-lang/discussions) or our [Discord](@{DISCORD}).

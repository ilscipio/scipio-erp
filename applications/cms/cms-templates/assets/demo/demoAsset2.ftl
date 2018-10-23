<#-- Demo asset template, dedicated to test cases -->


<@section title="Demo asset 2">
    <p>Test content: ${testContent!"missing"}</p>
    <p>demo_bool_1: ${(demo_bool_1!"missing")?string}</p>
    <p>demo_bool_2 (not empty parameters.demo_bool_2): ${(demo_bool_2!"missing")?string}</p>
    <p>demo_integer_1 (is number type? ${(demo_integer_1!false)?is_number?string}): ${demo_integer_1!"missing"}</p>
    <p>demo_double_1 (is number type? ${(demo_double_1!false)?is_number?string}): ${demo_double_1!"missing"}</p>
</@section>




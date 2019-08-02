pub fn map<T, U, F>(input: Vec<T>, mut function: F) -> Vec<U>
where
    F: FnMut(T) -> U,
{
    let mut v = Vec::with_capacity(input.len());
    for i in input {
        v.push(function(i));
    }

    v
}

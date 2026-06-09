Console.WriteLine("Hello, World!");

int i = 9;

string str = Convert.ToString(i);

{
    int result;
    var success = int.TryParse(str, out result);
    if (success)
    {
        Console.WriteLine($"result: {result}");
    }
}

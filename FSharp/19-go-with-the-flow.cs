using System;

namespace aochelper
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Startin");
            var r0 = 0;
            var limit = 10551361;

            for(var r1 = 1; r1 <= limit; r1++)
            {
                if (r1 % 10000 == 0)
                {
                    Console.WriteLine("R1: {0}, R0: {1}", r1, r0);
                }

                for(var r2 = 1; r2 <= limit / r1 + 1; r2++)
                {
                    if(limit == r1 * r2)
                    {
                        r0 += r1;
                    }
                }
            }

            Console.WriteLine("Done. R0: {0}", r0);
        }
    }
}

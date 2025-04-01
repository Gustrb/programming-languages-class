#include <stdlib.h>
#include <stdio.h>

#define GRID_SIZE 20
#define GENERATIONS 40

void print_grid(int grid[GRID_SIZE][GRID_SIZE])
{
    // clear
    printf("\033[H\033[J");

    for (size_t i = 0; i < GRID_SIZE; i++)
    {
        for (size_t j = 0; j < GRID_SIZE; j++)
        {
            if (grid[i][j] == 1)
                printf("O");
            else
                printf(".");
        }
        printf("\n");
    }
    printf("\n");
}

void clear_grid(int grid[GRID_SIZE][GRID_SIZE])
{
    for (size_t i = 0; i < GRID_SIZE; i++)
    {
        for (size_t j = 0; j < GRID_SIZE; j++)
        {
            grid[i][j] = 0;
        }
    }
}

void advance_generation(int grid[GRID_SIZE][GRID_SIZE], int new_grid[GRID_SIZE][GRID_SIZE])
{
    clear_grid(new_grid);

    for (size_t i = 0; i < GRID_SIZE; i++)
    {
        for (size_t j = 0; j < GRID_SIZE; j++)
        {
            size_t count = 0;
            size_t up, down, left, right;

            up = i -1;
            down = i + 1;
            left = j - 1;
            right = j + 1;

            if (up >= 0) count += grid[up][j];
            if (down < GRID_SIZE) count += grid[down][j];
            if (left >= 0) count += grid[i][left];
            if (right < GRID_SIZE) count += grid[i][right];


            if (down < GRID_SIZE && left >= 0) count += grid[down][left];
            if (down < GRID_SIZE && right < GRID_SIZE) count += grid[down][right];
            if (up >= 0 && left >= 0) count += grid[up][left];
            if (up >= 0 && right < GRID_SIZE) count += grid[up][right];

            if (grid[i][j] == 1)
            {
                if (count < 2 || count > 3)
                    new_grid[i][j] = 0;
                else
                    new_grid[i][j] = 1;
            }
            else
            {
                if (count == 3)
                    new_grid[i][j] = 1;
            }
        }
    }
}

int main(void)
{
    int grid[GRID_SIZE][GRID_SIZE] = {0};
    int new_grid[GRID_SIZE][GRID_SIZE] = {0};

    grid[4][4] = 1;
    grid[5][4] = 1;
    grid[6][4] = 1;
    grid[6][3] = 1;
    grid[5][2] = 1;

    for (size_t i = 0; i < GENERATIONS; i++)
    {
        print_grid(grid);
        advance_generation(grid, new_grid);

        // swap grids
        for (size_t j = 0; j < GRID_SIZE; j++)
        {
            for (size_t k = 0; k < GRID_SIZE; k++)
            {
                grid[j][k] = new_grid[j][k];
            }
        }

        usleep(100000); // sleep for 0.1 seconds
    }

    return EXIT_SUCCESS;
}

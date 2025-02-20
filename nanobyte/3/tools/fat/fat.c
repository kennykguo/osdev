// This is a driver interface for the FAT12 filesystem

#include <stdio.h>   // Standard input/output functions
#include <stdint.h>  // Standard integer types (e.g., uint8_t, uint16_t)
#include <stdlib.h>  // Standard library functions (e.g., malloc, free)
#include <string.h>  // String manipulation functions (e.g., memcmp)
#include <ctype.h>   // Character handling functions (e.g., isprint)

typedef uint8_t bool; // Define a boolean type using uint8_t
#define true 1       // Define true as 1
#define false 0      // Define false as 0

// Structure representing the Boot Sector of a FAT filesystem
typedef struct {
    uint8_t boot_jump_instruction[3]; // Jump instruction to boot code
    uint8_t oem_identifier[8];        // OEM identifier string
    uint16_t bytes_per_sector;        // Bytes per sector
    uint8_t sectors_per_cluster;      // Sectors per cluster
    uint16_t reserved_sectors;        // Reserved sectors count
    uint8_t fat_count;                // Number of FATs
    uint16_t dir_entry_count;         // Number of directory entries
    uint16_t total_sectors;           // Total sectors in the filesystem
    uint8_t media_descriptor_type;    // Media descriptor type
    uint16_t sectors_per_fat;         // Sectors per FAT
    uint16_t sectors_per_track;       // Sectors per track
    uint16_t heads;                   // Number of heads
    uint32_t hidden_sectors;          // Hidden sectors count
    uint32_t large_sector_count;      // Large sector count (if total_sectors is 0)

    // Extended boot record fields
    uint8_t drive_number;             // Drive number
    uint8_t _reserved;                // Reserved field
    uint8_t signature;                // Signature
    uint32_t volume_id;               // Volume serial number
    uint8_t volume_label[11];         // Volume label (11 bytes, padded with spaces)
    uint8_t system_id[8];             // System ID string
} __attribute__((packed)) BootSector; // Packed to avoid padding

// Structure representing a Directory Entry in the FAT filesystem
typedef struct {
    uint8_t name[11];                 // File name (8.3 format)
    // Only 11 characters allowed in the filename
    uint8_t attributes;               // File attributes
    uint8_t _reserved;                // Reserved field
    uint8_t created_time_tenths;      // Tenths of a second for creation time
    uint16_t created_time;            // Creation time
    uint16_t created_date;            // Creation date
    uint16_t accessed_date;           // Last accessed date
    uint16_t first_cluster_high;      // High 16 bits of the first cluster number
    uint16_t modified_time;           // Last modified time
    uint16_t modified_date;           // Last modified date
    uint16_t first_cluster_low;       // Low 16 bits of the first cluster number
    uint32_t size;                    // File size in bytes
} __attribute__((packed)) DirectoryEntry; // Packed to avoid padding

// Global variables
BootSector g_boot_sector;             // Boot sector of the disk
uint8_t* g_fat = NULL;                // File Allocation Table (FAT)
DirectoryEntry* g_root_directory = NULL; // Root directory entries
uint32_t g_root_directory_end;        // End of the root directory in sectors

// Function to read the boot sector from the disk
bool read_boot_sector(FILE* disk) {
    // Reads sizeof(g_boot_sector) bytes from the file (disk) into the g_boot_sector structure.
    return fread(&g_boot_sector, sizeof(g_boot_sector), 1, disk) > 0;
}


// Function to read a specified number of sectors from the disk, starting at some sector, which also starts at zero
// Returns bool status if completed or not
bool read_sectors(FILE* disk, uint32_t lba, uint32_t count, void* buffer_out) {
    bool ok = true;
    // Moves the file pointer to the Logical Block Address (LBA) multiplied by the bytes per sector.
    ok = ok && (fseek(disk, lba * g_boot_sector.bytes_per_sector, SEEK_SET) == 0);

    // Read sector's number of bytes count total times
    ok = ok && (fread(buffer_out, g_boot_sector.bytes_per_sector, count, disk) == count);

    // Guessing that the code wants us to return a status
    return ok;
}

// Function to read the entire FAT from the disk (reads the entire FAT floppy disk)
bool read_fat(FILE* disk) {
    // Allocating enough memory bytes to read from the FAT part of the disk
    // Allocate enough memory specifically for the total sectors taken up by the FAT part of the structure
    g_fat = (uint8_t*) malloc(g_boot_sector.sectors_per_fat * g_boot_sector.bytes_per_sector); 
    // Starting from how many reserved sectors their are -> logical block address, we read a set # of sectors
    return read_sectors(disk, g_boot_sector.reserved_sectors, g_boot_sector.sectors_per_fat, g_fat); 
}

// Function to read the root directory from the disk
bool read_root_directory(FILE* disk) {
    // Calculate LBA - skip the boot sector, and move past through to the end of the FAT area
    uint32_t lba = g_boot_sector.reserved_sectors + g_boot_sector.sectors_per_fat * g_boot_sector.fat_count; 
    // Calculate size of root directory
    uint32_t size = sizeof(DirectoryEntry) * g_boot_sector.dir_entry_count; 
    // Calculate number of sectors
    uint32_t sectors = (size / g_boot_sector.bytes_per_sector); 

    // Adjust sectors if there's a remainder
    if (size % g_boot_sector.bytes_per_sector > 0)
        sectors++;

    // Set the end of the root directory
    // Is a sector # -> lba holds until the end of the fat, sectors holds how many sectors are in the root directory
    g_root_directory_end = lba + sectors; 


    // ALlocate the correct amount of memory for the root directory
    g_root_directory = (DirectoryEntry*) malloc(sectors * g_boot_sector.bytes_per_sector);
    
    // Read root directory sectors
    return read_sectors(disk, lba, sectors, g_root_directory); 
}

// Function to find a file in the root directory by name
DirectoryEntry* find_file(const char* name) {
    // Loop over all directory entry counts
    for (uint32_t i = 0; i < g_boot_sector.dir_entry_count; i++) {
        // Index directly into the struct holding the directory, and get the name, then  compare its name (11 characters) with memcmp
        if (memcmp(name, g_root_directory[i].name, 11) == 0) // Compare file names
            return &g_root_directory[i]; // Return the directory entry if found
    }
    return NULL; // Return NULL if the file is not found
}

bool read_file(DirectoryEntry* file_entry, FILE* disk, uint8_t* output_buffer) {
    // Initialize a boolean variable to track the success of file reading operations.
    bool ok = true;

    // Start with the first cluster of the file. The starting cluster is stored in the directory entry of the struct data structure
    // For FAT12, the first cluster number is stored in the `first_cluster_low` field of the DirectoryEntry structure.
    uint16_t current_cluster = file_entry->first_cluster_low;

    // Begin a loop to read the file's data cluster by cluster.
    do {
        // Calculate the Logical Block Address (LBA) for the current cluster.
        // - `g_root_directory_end` is the LBA where the data region starts (after the root directory).
        // - `(current_cluster - 2)` adjusts the cluster number because cluster numbering starts at 2 - since (Cluster 0 and 1 are reserved in FAT12.)
        // - `g_boot_sector.sectors_per_cluster` is the number of sectors per cluster.
        uint32_t lba = g_root_directory_end + (current_cluster - 2) * g_boot_sector.sectors_per_cluster;

        // Read the current cluster from the disk into the output buffer.
        // We are reading a single cluster's worth of sectors into output_buffer
        ok = ok && read_sectors(disk, lba, g_boot_sector.sectors_per_cluster, output_buffer);

        // Move the output buffer pointer forward by the size of the cluster just read
        output_buffer += g_boot_sector.sectors_per_cluster * g_boot_sector.bytes_per_sector;

        // Calculate the index in the FAT for the current cluster.
        // - FAT12 uses 12 bits (1.5 bytes) per cluster entry.
        // - `current_cluster * 3 / 2` calculates the byte offset in the FAT for the current cluster.
        // Converting cluster index to 
        uint32_t fat_index = current_cluster * 3 / 2;

        // Determine the next cluster in the chain.
        // - If the current cluster is even:
        if (current_cluster % 2 == 0)
            // For even clusters, the next cluster is stored in the lower 12 bits of the 16-bit FAT entry.
            // - `*(uint16_t*)(g_fat + fat_index)` reads the 16-bit value at the calculated FAT index.
            // - `& 0x0FFF` masks out the upper 4 bits to get the lower 12 bits.
            current_cluster = (*(uint16_t*)(g_fat + fat_index)) & 0x0FFF;
        else
            // For odd  clusters, the next cluster is stored in the upper 12 bits of the 16-bit FAT entry.
            // - `>> 4` shifts the 16-bit value right by 4 bits to get the upper 12 bits.
            current_cluster = (*(uint16_t*)(g_fat + fat_index)) >> 4;

    // Continue the loop until:
    // - `ok` is false (indicating a read error), or
    // - `current_cluster` is greater than or equal to `0x0FF8` (indicating the end of the cluster chain).
    // In FAT12, cluster values `0xFF8` to `0xFFF` indicate the end of a file.
    } while (ok && current_cluster < 0x0FF8);

    // Return the status of the file read operation.
    // - `true` if the file was read successfully.
    // - `false` if an error occurred during reading.
    return ok;
}

// Main function
int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Syntax: %s <disk image> <file name>\n", argv[0]); // Print usage syntax
        return -1;
    }
    // Opens the file in binary mode
    FILE* disk = fopen(argv[1], "rb");


    if (!disk) {
        fprintf(stderr, "Cannot open disk image %s!\n", argv[1]); // Print error if the file cannot be opened
        return -1;
    }

    if (!read_boot_sector(disk)) {
        fprintf(stderr, "Could not read boot sector!\n"); // Print error if the boot sector cannot be read
        return -2;
    }

    if (!read_fat(disk)) {
        fprintf(stderr, "Could not read FAT!\n"); // Print error if the FAT cannot be read
        free(g_fat);
        return -3;
    }

    if (!read_root_directory(disk)) {
        fprintf(stderr, "Could not read FAT!\n"); // Print error if the root directory cannot be read
        free(g_fat);
        free(g_root_directory);
        return -4;
    }

    // Argument 2 is the file
    // Find the file in the root directory
    DirectoryEntry* file_entry = find_file(argv[2]); 
    if (!file_entry) {
        fprintf(stderr, "Could not find file %s!\n", argv[2]); // Print error if the file is not found
        free(g_fat);
        free(g_root_directory);
        return -5;
    }

    uint8_t* buffer = (uint8_t*) malloc(file_entry->size + g_boot_sector.bytes_per_sector); // Allocate memory for the file
    if (!read_file(file_entry, disk, buffer)) {
        fprintf(stderr, "Could not read file %s!\n", argv[2]); // Print error if the file cannot be read
        free(g_fat);
        free(g_root_directory);
        free(buffer);
        return -5;
    }

    for (size_t i = 0; i < file_entry->size; i++) {
        if (isprint(buffer[i])) fputc(buffer[i], stdout); // Print printable characters
        else printf("<%02x>", buffer[i]); // Print non-printable characters as hex
    }

    printf("\n");

    free(buffer); // Free the buffer
    free(g_fat); // Free the FAT
    free(g_root_directory); // Free the root directory
    return 0;
}
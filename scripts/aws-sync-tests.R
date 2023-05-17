#renv::install("ecohealthalliance/containerTemplateUtils@feature/aws_s3_download")
#debugonce(aws_s3_download)

library(containerTemplateUtils)

aws_bucket <- "project-dtra-ml-main"
top_dir <- "dir1" # this is a directory in AWS, but not local
local_path <- "dir2/dir3" # this exists locally 
list.files(local_path)

# this has expected behavior -- uploads folder to "project-dtra-ml-main/dir1/dir2/dir3"
aws_s3_upload(path = local_path,
              bucket =  aws_bucket ,
              key = local_path, 
              prefix = paste0(top_dir, "/"),
              check = TRUE)

# this does not honor the top_dir, it creates a folder called dir2 in project-dtra-ml-main
aws_s3_upload(path = local_path ,
              bucket =  aws_bucket ,
              key = paste(top_dir,local_path, sep = "/") , 
              check = TRUE)

# this recreates my full here path within the bucket (Users/emmamendelsohn etc etc)
aws_s3_upload(path = here::here(local_path) ,
              bucket =  aws_bucket ,
              key = paste(top_dir,local_path, sep = "/") , 
              check = TRUE)

# this works now!
aws_s3_download(path = local_path,
                bucket =  aws_bucket,
                key = paste(top_dir,local_path, sep = "/"), 
                check = TRUE)

aws_s3_download(path = here::here(local_path),
                bucket =  aws_bucket,
                key = paste(top_dir,local_path, sep = "/"), 
                check = TRUE)

# this also works as expected (creates filepath dir2/dir3/dir1/dir2/dir3)
aws_s3_download(path = local_path,
                bucket =  aws_bucket,
                key = paste(top_dir,local_path, sep = "/"), 
                check = TRUE, 
                copy_s3_dir_structure = TRUE)





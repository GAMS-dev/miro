set fil(*);
scalar z_sum, f_sum;

$onEmbeddedCode Python:
import os
with open('hcube_file_names.txt', 'r') as f:
   files = f.readlines()
   file_names = [os.path.splitext(x)[0] for x in files]
   os.environ["GDX_FILENAMES"]='"' + '" "'.join(files) + '"'
gams.set("fil", file_names)
$offEmbeddedCode fil


parameter z_batch(fil), f_batch(fil);

$call gdxmerge %sysEnv.GDX_FILENAMES% output=batch_analysis

$gdxin batch_analysis
$load z_batch=total_cost, f_batch=f
$gdxin


z_sum=sum(fil, z_batch(fil));
f_sum=sum(fil, f_batch(fil));


file test /test.md/;
put test
put "# Total transportation cost: ", z_sum /;
put "**Total freight cost: ", f_sum, '**';

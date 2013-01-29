package nl.vu.few;

import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import org.apache.pig.data.BagFactory;
import org.apache.pig.data.DataBag;
import org.apache.pig.PigWarning;
import org.apache.pig.EvalFunc;
import org.apache.pig.data.DataType;
import org.apache.pig.data.Tuple;
import org.apache.pig.data.TupleFactory;
import org.apache.pig.impl.logicalLayer.schema.Schema;


import org.jsoup.Jsoup;
import org.jsoup.helper.Validate;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;


public class ParseHTML extends EvalFunc<Tuple> {
	@Override
	public Tuple exec(Tuple input) throws IOException {
		if (input == null || input.size() == 0 || input.get(0) == null)
			return null;

		String html = null;

		try {
			html = (String) input.get(0);
		} catch (ClassCastException e) {
			warn("unable to cast input "+input.get(0)+" of class "+
				input.get(0).getClass()+" to String", PigWarning.UDF_WARNING_1);
			return null;
		}
		Document doc = Jsoup.parse(html);
		Element body = doc.body();

		if (body == null)
			return null;


		String out = body.text().replaceAll("\\p{Cntrl}", "").replaceAll("\\p{C}", "?");


		Elements links = doc.select("a[href]");


		// doe er iets mee
		String title = null;
		Element t = doc.select("title").first();
		if (t != null)
			title = t.text().replaceAll("\\p{Cntrl}", "").replaceAll("\\p{C}", "?");

		DataBag links_bag = BagFactory.getInstance().newDefaultBag();
		for (Element link : links) {
			if (link.text().trim() != null 
				&& link.text().trim() != "" 
				&& link.attr("abs:href") != null 
				&& link.attr("abs:href") != "") {
				Tuple link_t = TupleFactory.getInstance().newTuple(2);
				link_t.set(0,link.text().trim());
				link_t.set(1,link.attr("abs:href"));
				links_bag.add(link_t);
			}
		}

		Tuple ret = TupleFactory.getInstance().newTuple(3);
		ret.set(0,title);
		ret.set(1,out);
		ret.set(2,links_bag);
		return ret;
	}

	// @Override
	// public Schema outputSchema(Schema input) {
	// 	try {
	// 		Schema tupleSchema = new Schema();
	// 		for (int i = 0; i < input.size(); ++i) {
	// 			tupleSchema.add(input.getField(i));
	// 		}
	// 		return new Schema(new Schema.FieldSchema(getSchemaName(this
	// 				.getClass().getName().toLowerCase(), input), tupleSchema,
	// 				DataType.TUPLE));
	// 	} catch (Exception e) {
	// 		return null;
	// 	}
	// }
}